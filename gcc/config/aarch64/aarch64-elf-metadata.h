/* ELF metadata for AArch64 architecture.
   Copyright (C) 2024-2026 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_AARCH64_ELF_METADATA_H
#define GCC_AARCH64_ELF_METADATA_H

#include "vec.h"

namespace aarch64 {

class section_note_gnu_property
{
 public:
  section_note_gnu_property ();

  /* Add BTI flag to GNU properties.  */
  void bti_enabled ();
  /* Add GCS flag to GNU properties.  */
  void gcs_enabled ();
  /* Add PAC flag to GNU properties.  */
  void pac_enabled ();

  /* Write the data to the assembly file.  */
  void write () const;

 private:
  unsigned m_feature_1_and;
};

enum subsection_optionality : uint8_t
{
  required = 0x0,
  optional = 0x1,
};

enum subsection_val_type : uint8_t
{
  uleb128 = 0x0,
  ntbs = 0x1,
};

enum BA_TagFeature_t : uint8_t
{
  Tag_Feature_BTI = 0,
  Tag_Feature_PAC = 1,
  Tag_Feature_GCS = 2,
};

template <typename T_tag, typename T_val>
struct aeabi_attribute
{
  T_tag tag;
  T_val value;
};

template <typename T_tag, typename T_val>
aeabi_attribute<T_tag, T_val>
make_aeabi_attribute (T_tag tag, T_val val)
{
  return aeabi_attribute<T_tag, T_val>{tag, val};
}

namespace details {

constexpr const char *
to_c_str (bool b)
{
  return b ? "true" : "false";
}

constexpr const char *
to_c_str (const char *s)
{
  return s;
}

constexpr const char *
to_c_str (subsection_optionality v)
{
  return (v == optional ? "optional"
	  : v == required ? "required"
	  : nullptr);
}

constexpr const char *
to_c_str (subsection_val_type v)
{
  return (v == uleb128 ? "ULEB128"
	  : v == ntbs ? "NTBS"
	  : nullptr);
}

constexpr const char *
to_c_str (BA_TagFeature_t feature)
{
  return (feature == Tag_Feature_BTI ? "Tag_Feature_BTI"
	  : feature == Tag_Feature_PAC ? "Tag_Feature_PAC"
	  : feature == Tag_Feature_GCS ? "Tag_Feature_GCS"
	  : nullptr);
}

template <
  typename T,
  typename = typename std::enable_if<std::is_unsigned<T>::value, T>::type
>
constexpr const char *
aeabi_attr_str_fmt (T)
{
  return "\t.aeabi_attribute %s, %u";
}

constexpr const char *
aeabi_attr_str_fmt (const char *)
{
  return "\t.aeabi_attribute %s, \"%s\"";
}

template <
  typename T,
  typename = typename std::enable_if<std::is_unsigned<T>::value, T>::type
>
constexpr uint8_t
aeabi_attr_val_for_fmt (T value)
{
  return static_cast<uint8_t>(value);
}

constexpr const char *
aeabi_attr_val_for_fmt (const char *s)
{
  return s;
}

template <typename T_tag, typename T_val>
void
write (FILE *out_file, aeabi_attribute<T_tag, T_val> const &attr)
{
  asm_fprintf (out_file, aeabi_attr_str_fmt (T_val{}),
	       to_c_str (attr.tag), aeabi_attr_val_for_fmt (attr.value));
  if (flag_debug_asm)
    asm_fprintf (out_file, "\t%s %s: %s", ASM_COMMENT_START,
		 to_c_str (attr.tag), to_c_str (attr.value));
  asm_fprintf (out_file, "\n");
}

template <
  typename T,
  typename = typename std::enable_if<std::is_unsigned<T>::value, T>::type
>
constexpr subsection_val_type
deduce_attr_av_type (T)
{
  return subsection_val_type::uleb128;
}

constexpr subsection_val_type
deduce_attr_av_type (const char *)
{
  return subsection_val_type::ntbs;
}

} // namespace details

/* AEABI subsections can be public or private.  A subsection is public if it is
   prefixed with "aeabi", private otherwise.  The header of an AEABI subsection
   is composed of a name (usually a vendor name), an optionality status (optional
   or required), and the expected type of its associated attributes (ULEB128 or
   NTBS).  Note: The attributes in the same subsection have all the same type.
   An attribute is composed of a tag identifier (ULEB128), and its value (ULEB128
   or NTBS).

   Syntax:
     .aeabi_subsection NameOfTheSubsection: string (=NTBS),
		       Optional: boolean (=ULEB128),
		       AttributeValueType: enum{ULEB128, NTBS} (=ULEB128)
     [
       .aeabi_attribute  TagIdentifier: ULEB128,
			 TagValue: Variant[ULEB128|NTBS]
     ]*

   Example:
     .aeabi_subsection .aeabi-feature-and-bits, optional, ULEB128
     .aeabi_attribute Tag_Feature_GCS, 1 // Tag_Feature_GCS: true

   Note: The textual representations of the tag and its value are emitted as a
   comment along their numerical representations to annotate the assembler
   output when the developer flag '-dA' is provided.  */
template <
  typename T_tag, /* The type of a tag.  */
  typename T_val, /* The type of a value.  */
  size_t N = 0    /* The number of expected attributes if we know it.  */
>
class aeabi_subsection
{
 public:
  aeabi_subsection (const char *name, bool optional)
    : m_name (name),
      m_optionality (optional
		     ? subsection_optionality::optional
		     : subsection_optionality::required),
      m_avtype (details::deduce_attr_av_type (T_val{}))
  {}

  /* Append an attribute to the subsection.  */
  void append (aeabi_attribute<T_tag, T_val> &&attr)
  {
    m_attributes.quick_push (std::move (attr));
  }

  /* Write the data to the assembly file.  */
  void write (FILE *out_file) const
  {
    asm_fprintf (out_file, "\n\t.aeabi_subsection %s, %s, %s\n",
		 m_name, details::to_c_str (m_optionality),
		 details::to_c_str (m_avtype));

    for (auto const &attr : m_attributes)
      details::write (out_file, attr);
  }

  /* Indicate if the subsection is empty.  */
  bool empty () const
  {
    return m_attributes.is_empty ();
  }

 private:
  const char *m_name;
  subsection_optionality m_optionality;
  subsection_val_type m_avtype;
  auto_vec<aeabi_attribute<T_tag, T_val>, N> m_attributes;
};

} // namespace aarch64

#endif /* GCC_AARCH64_ELF_METADATA_H */

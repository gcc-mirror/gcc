/* new abi support -*- C++ -*-
   Copyright (C) 2000
   Free Software Foundation, Inc.
   Written by Nathan Sidwell, Codesourcery LLC, <nathan@codesourcery.com>

   This file declares the new abi entry points into the runtime. It is not
   normally necessary for user programs to include this header, or use the
   entry points directly. However, this header is available should that be
   needed.
   
   Some of the entry points are intended for both C and C++, thus this header
   is includable from both C and C++. Though the C++ specific parts are not
   available in C, naturally enough.  */

#ifndef __CXXABI_H
#define __CXXABI_H 1

#if defined(__cplusplus) && (!defined(__GXX_ABI_VERSION) || __GXX_ABI_VERSION < 100)
/* These structures only make sense when targeting the new abi, catch a
   bonehead error early rather than let the user get very confused.  */
#error "Not targetting the new abi, supply -fnew-abi"
#endif

#ifdef __cplusplus

#include <typeinfo>
#include <stddef.h>

namespace __cxxabiv1
{

/* type information for int, float etc */
class __fundamental_type_info
  : public std::type_info
{
public:
  virtual ~__fundamental_type_info ();
public:
  explicit __fundamental_type_info (const char *n_)
    : std::type_info (n_)
    { }
};

/* type information for pointer to data or function, but not pointer to member */
class __pointer_type_info
  : public std::type_info
{
/* abi defined member variables */
public:
  int quals;                    /* qualification of the target object */
  const std::type_info *type;   /* type of pointed to object */

/* abi defined member functions */
public:
  virtual ~__pointer_type_info ();
public:
  explicit __pointer_type_info (const char *n_,
                                int quals_,
                                const std::type_info *type_)
    : std::type_info (n_), quals (quals_), type (type_)
    { }

/* implementation defined types */
public:
  enum quals_masks {
    const_mask = 0x1,
    volatile_mask = 0x2
  };

/* implementation defined member functions */
protected:
  virtual bool is_pointer_p () const;
protected:
  virtual bool do_catch (const std::type_info *thr_type, void **thr_obj,
                         unsigned outer) const;
};

/* type information for array objects */
class __array_type_info
  : public std::type_info
{
/* abi defined member functions */
protected:
  virtual ~__array_type_info ();
public:
  explicit __array_type_info (const char *n_)
    : std::type_info (n_)
    { }
};

/* type information for functions (both member and non-member) */
class __function_type_info
  : public std::type_info
{
/* abi defined member functions */
public:
  virtual ~__function_type_info ();
public:
  explicit __function_type_info (const char *n_)
    : std::type_info (n_)
    { }
  
/* implementation defined member functions */
protected:
  virtual bool is_function_p () const;
};

/* type information for enumerations */
class __enum_type_info
  : public std::type_info
{
/* abi defined member functions */
public:
  virtual ~__enum_type_info ();
public:
  explicit __enum_type_info (const char *n_)
    : std::type_info (n_)
    { }
};

/* type information for a pointer to member variable (not function) */
class __pointer_to_member_type_info
  : public std::type_info
{
/* abi defined member variables */
public:
  const __class_type_info *klass;   /* class of the member */
  const std::type_info *type;       /* type of the pointed to member */
  int quals;                        /* qualifications of the pointed to type */

/* abi defined member functions */
public:
  virtual ~__pointer_to_member_type_info ();
public:
  explicit __pointer_to_member_type_info (const char *n_,
                                          const __class_type_info *klass_,
                                          const std::type_info *type_,
                                          int quals_)
    : std::type_info (n_), klass (klass_), type (type_), quals (quals_)
    { }

/* implementation defined types */
public:
  enum quals_masks {
    const_mask = 0x1,
    volatile_mask = 0x2
  };

/* implementation defined member functions */
protected:
  virtual bool do_catch (const std::type_info *thr_type, void **thr_obj,
                         unsigned outer) const;
};

class __class_type_info;

/* helper class for __vmi_class_type */
class __base_class_info
{
/* abi defined member variables */
public:
  const __class_type_info *base;    /* base class type */
  long vmi_offset_flags;            /* offset and info */

/* implementation defined types */
public:
  enum vmi_masks {
    virtual_mask = 0x1,
    public_mask = 0x2,
    hwm_bit = 2,
    offset_shift = 8          /* bits to shift offset by */
  };
  
/* implementation defined member functions */
public:
  bool is_virtual_p () const
    { return vmi_offset_flags & virtual_mask; }
  bool is_public_p () const
    { return vmi_offset_flags & public_mask; }
  std::ptrdiff_t offset () const
    { return std::ptrdiff_t (vmi_offset_flags) >> offset_shift; }
};

/* type information for a class */
class __class_type_info
  : public std::type_info
{
/* abi defined member functions */
public:
  virtual ~__class_type_info ();
public:
  explicit __class_type_info (const char *n_)
    : type_info (n_)
    { }

/* implementation defined types */
public:
  /* sub_kind tells us about how a base object is contained within a derived
     object. We often do this lazily, hence the UNKNOWN value. At other times
     we may use NOT_CONTAINED to mean not publicly contained. */
  enum sub_kind
  {
    unknown = 0,              /* we have no idea */
    not_contained,            /* not contained within us (in some */
                              /* circumstances this might mean not contained */
                              /* publicly) */
    contained_ambig,          /* contained ambiguously */
    
    contained_virtual_mask = __base_class_info::virtual_mask, /* via a virtual path */
    contained_public_mask = __base_class_info::public_mask,   /* via a public path */
    contained_mask = 1 << __base_class_info::hwm_bit,         /* contained within us */
    
    contained_private = contained_mask,
    contained_public = contained_mask | contained_public_mask
  };

public:  
  struct upcast_result
  {
    const void *dst_ptr;        /* pointer to caught object */
    sub_kind whole2dst;         /* path from most derived object to target */
    int src_details;            /* hints about the source type heirarchy */
    const __class_type_info *base_type; /* where we found the target, */
                                /* if in vbase the __class_type_info of vbase */
                                /* if a non-virtual base then 1 */
                                /* else NULL */
    public:
    upcast_result (int d)
      :dst_ptr (NULL), whole2dst (unknown), src_details (d), base_type (NULL)
      {}
  };

public:
  /* dyncast_result is used to hold information during traversal of a class
     heirarchy when dynamic casting. */
  struct dyncast_result
  {
    const void *dst_ptr;        /* pointer to target object or NULL */
    sub_kind whole2dst;         /* path from most derived object to target */
    sub_kind whole2src;         /* path from most derived object to sub object */
    sub_kind dst2src;           /* path from target to sub object */
    
    public:
    dyncast_result ()
      :dst_ptr (NULL), whole2dst (unknown),
       whole2src (unknown), dst2src (unknown)
      {}
  };

/* implementation defined member functions */
protected:
  virtual bool do_upcast (const __class_type_info *dst_type, void **obj_ptr) const;

protected:
  virtual bool do_catch (const type_info *thr_type, void **thr_obj,
                         unsigned outer) const;


public:
  /* Helper for upcast. See if DST is us, or one of our bases. ACCESS_PATH */
  /* gives the access from the start object. Return TRUE if we know the upcast */
  /* fails. */
  virtual bool do_upcast (sub_kind access_path,
                          const __class_type_info *dst, const void *obj,
                          upcast_result &__restrict result) const;

public:
  /* Indicate whether SRC_PTR of type SRC_TYPE is contained publicly within
     OBJ_PTR. OBJ_PTR points to a base object of our type, which is the
     destination type. SRC2DST indicates how SRC objects might be contained
     within this type.  If SRC_PTR is one of our SRC_TYPE bases, indicate the
     virtuality. Returns not_contained for non containment or private
     containment. */
  inline sub_kind find_public_src (std::ptrdiff_t src2dst, const void *obj_ptr,
                                   const __class_type_info *src_type,
                                   const void *src_ptr) const;

public:
  /* dynamic cast helper. ACCESS_PATH gives the access from the most derived
     object to this base. DST_TYPE indicates the desired type we want. OBJ_PTR
     points to a base of our type within the complete object. SRC_TYPE
     indicates the static type started from and SRC_PTR points to that base
     within the most derived object. Fill in RESULT with what we find. Return
     true if we have located an ambiguous match. */
  virtual bool do_dyncast (std::ptrdiff_t src2dst, sub_kind access_path,
                           const __class_type_info *dst_type, const void *obj_ptr,
                           const __class_type_info *src_type, const void *src_ptr,
                           dyncast_result &result) const;
public:
  /* Helper for find_public_subobj. SRC2DST indicates how SRC_TYPE bases are
     inherited by the type started from -- which is not necessarily the
     current type. The current type will be a base of the destination type.
     OBJ_PTR points to the current base. */
  virtual sub_kind do_find_public_src (std::ptrdiff_t src2dst, const void *obj_ptr,
                                       const __class_type_info *src_type,
                                       const void *src_ptr) const;
};

/* type information for a class with a single non-virtual base */
class __si_class_type_info
  : public __class_type_info
{
/* abi defined member variables */
protected:
  const __class_type_info *base;    /* base type */

/* abi defined member functions */
public:
  virtual ~__si_class_type_info ();
public:
  explicit __si_class_type_info (const char *n_,
                                 const __class_type_info *base_)
    : __class_type_info (n_), base (base_)
    { }

/* implementation defined member functions */
protected:
  virtual bool do_dyncast (std::ptrdiff_t src2dst, sub_kind access_path,
                           const __class_type_info *dst_type, const void *obj_ptr,
                           const __class_type_info *src_type, const void *src_ptr,
                           dyncast_result &result) const;
  virtual sub_kind do_find_public_src (std::ptrdiff_t src2dst, const void *obj_ptr,
                                       const __class_type_info *src_type,
                                       const void *sub_ptr) const;
  virtual bool do_upcast (sub_kind access_path,
                          const __class_type_info *dst, const void *obj,
                          upcast_result &__restrict result) const;
};

/* type information for a class with multiple and/or virtual bases */
class __vmi_class_type_info : public __class_type_info {
/* abi defined member variables */
public:
  int details;      /* details about the class heirarchy */
  int n_bases;      /* number of direct bases */
  __base_class_info base_list[1]; /* array of bases */
  /* The array of bases uses the trailing array struct hack
     so this class is not constructable with a normal constructor. It is
     internally generated by the compiler. */

/* abi defined member functions */
public:
  virtual ~__vmi_class_type_info ();
public:
  explicit __vmi_class_type_info (const char *n_,
                                  int details_)
    : __class_type_info (n_), details (details_), n_bases (0)
    { }

/* implementation defined types */
public:
  enum detail_masks {
    non_diamond_repeat_mask = 0x1,   /* distinct instance of repeated base */
    diamond_shaped_mask = 0x2,       /* diamond shaped multiple inheritance */
    non_public_base_mask = 0x4,      /* has non-public direct or indirect base */
    public_base_mask = 0x8,          /* has public base (direct) */
    
    details_unknown_mask = 0x10
  };

/* implementation defined member functions */
protected:
  virtual bool do_dyncast (std::ptrdiff_t src2dst, sub_kind access_path,
                           const __class_type_info *dst_type, const void *obj_ptr,
                           const __class_type_info *src_type, const void *src_ptr,
                           dyncast_result &result) const;
  virtual sub_kind do_find_public_src (std::ptrdiff_t src2dst, const void *obj_ptr,
                                       const __class_type_info *src_type,
                                       const void *src_ptr) const;
  virtual bool do_upcast (sub_kind access_path,
                          const __class_type_info *dst, const void *obj,
                          upcast_result &__restrict result) const;
};

/* dynamic cast runtime */
void *__dynamic_cast (const void *src_ptr,    /* object started from */
                      const __class_type_info *src_type, /* static type of object */
                      const __class_type_info *dst_type, /* desired target type */
                      std::ptrdiff_t src2dst); /* how src and dst are related */

    /* src2dst has the following possible values
       >= 0: src_type is a unique public non-virtual base of dst_type
             dst_ptr + src2dst == src_ptr
       -1: unspecified relationship
       -2: src_type is not a public base of dst_type
       -3: src_type is a multiple public non-virtual base of dst_type */



} /* namespace __cxxabiv1 */

/* User programs should use the alias `abi'. */
namespace abi = __cxxabiv1;

#else
#endif /* __cplusplus */


#endif /* __CXXABI_H */

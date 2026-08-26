// optional<T&> -*- C++ -*-

// Copyright (C) 2013-2026 Free Software Foundation, Inc.
// Copyright The GNU Toolchain Authors.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

#ifndef GCC_BACKPORT_GLIBCXX_OPTIONAL_REF
#define GCC_BACKPORT_GLIBCXX_OPTIONAL_REF 1

#include <type_traits>
#include <utility>
#include "inplace_tags.h"

namespace gcc
{

   /**
   *  @addtogroup utilities
   *  @{
   */

  template<typename G_Tp>
    class optional;

  /// Tag type to disengage optional objects.
  struct nullopt_t
  {
    // Do not user-declare default constructor at all for
    // optional_value = {} syntax to work.
    // nullopt_t() = delete;

    // Used for constructing nullopt.
    enum class G_Construct { G_Token };

    // Must be constexpr for nullopt_t to be literal.
    explicit constexpr nullopt_t(G_Construct) noexcept { }
  };

  /// Tag to disengage optional objects.
  static constexpr nullopt_t nullopt { nullopt_t::G_Construct::G_Token };

  template<typename G_Fn> struct G_Optional_func { G_Fn& m_f; };

  template<typename G_Tp>
    static constexpr bool g_is_valid_contained_type_for_optional =
      (
	std::is_lvalue_reference<G_Tp>::value ||
	(std::is_object<G_Tp>::value && std::is_destructible<G_Tp>::value && !std::is_array<G_Tp>::value)
      )
      && !std::is_same<std::remove_cv_t<std::remove_reference_t<G_Tp>>, nullopt_t>::value
      && !std::is_same<std::remove_cv_t<std::remove_reference_t<G_Tp>>, in_place_t>::value;

  template<typename G_Tp>
    class optional<G_Tp&>;

  template<typename G_Tp>
  struct g_is_optional_ref : std::false_type {};

  template<typename G_Tp>
  struct g_is_optional_ref<optional<G_Tp&>> : std::true_type {};

  template<typename G_Tp>
    struct g_optional_ref_base
    {};

  template<typename G_Tp>
    class optional<G_Tp&> : public g_optional_ref_base<G_Tp>
    {
      static_assert(g_is_valid_contained_type_for_optional<G_Tp&>,
		    "Given type not valid for an optional");

    public:
      using value_type = G_Tp;

      constexpr static optional
      G_S_from_ptr(G_Tp* g_ptr)
      {
	optional g_res;
	g_res.m_val = g_ptr;
	return g_res;
      }

      // Constructors.
      constexpr optional() noexcept = default;
      constexpr optional(nullopt_t) noexcept : optional() {}
      constexpr optional(const optional&) noexcept = default;

      template<typename G_Arg,
	       G_Requires<
		 std::is_constructible<G_Tp&, G_Arg>,
		 not_<reference_constructs_from_temporary<G_Tp&, G_Arg>>
		 > = false>
	explicit constexpr
	optional(in_place_t, G_Arg&& g_arg)
	{
	  g_convert_ref_init_val(std::forward<G_Arg>(g_arg));
	}

      template<typename G_Up,
	       G_Requires<
		 not_<std::is_same<remove_cvref_t<G_Up>, optional>>,
		 not_<std::is_same<remove_cvref_t<G_Up>, in_place_t>>,
		 std::is_constructible<G_Tp&, G_Up>,
		 not_<reference_constructs_from_temporary<G_Tp&, G_Up>>,

		 // Explicit:
		 not_<std::is_convertible<G_Up, G_Tp&>>
		 > = false>
	explicit
	constexpr
	optional(G_Up&& g_u)
	noexcept(std::is_nothrow_constructible<G_Tp&, G_Up>::value)
	{
	  g_convert_ref_init_val(std::forward<G_Up>(g_u));
	}
      template<typename G_Up,
	       G_Requires<
		 not_<std::is_same<remove_cvref_t<G_Up>, optional>>,
		 not_<std::is_same<remove_cvref_t<G_Up>, in_place_t>>,
		 std::is_constructible<G_Tp&, G_Up>,
		 not_<reference_constructs_from_temporary<G_Tp&, G_Up>>,

		 // !Explicit:
		 std::is_convertible<G_Up, G_Tp&>
		 > = false>
	constexpr
	optional(G_Up&& g_u)
	noexcept(std::is_nothrow_constructible<G_Tp&, G_Up>::value)
	{
	  g_convert_ref_init_val(std::forward<G_Up>(g_u));
	}

      template<typename G_Up,
	       G_Requires<
		 not_<std::is_same<remove_cvref_t<G_Up>, optional>>,
		 not_<std::is_same<remove_cvref_t<G_Up>, in_place_t>>,
		 std::is_constructible<G_Tp&, G_Up>,
		 reference_constructs_from_temporary<G_Tp&, G_Up>,

		 // Explicit:
		 not_<std::is_convertible<G_Up, G_Tp&>>
		 > = false>
	explicit
	constexpr
	optional(G_Up&& g_u) = delete;
      template<typename G_Up,
	       G_Requires<
		 not_<std::is_same<remove_cvref_t<G_Up>, optional>>,
		 not_<std::is_same<remove_cvref_t<G_Up>, in_place_t>>,
		 std::is_constructible<G_Tp&, G_Up>,
		 reference_constructs_from_temporary<G_Tp&, G_Up>,

		 // !Explicit:
		 std::is_convertible<G_Up, G_Tp&>
		 > = false>
	constexpr
	optional(G_Up&& g_u) = delete;

      // optional<U> &
      template<typename G_Up,
	       G_Requires<
		 not_<std::is_same<std::remove_cv_t<G_Tp>, optional<G_Up>>>,
		 not_<std::is_same<G_Tp&, G_Up>>,
		 std::is_constructible<G_Tp&, G_Up&>,
		 not_<reference_constructs_from_temporary<G_Tp&, G_Up&>>,

		 // Explicit:
		 not_<std::is_convertible<G_Up&, G_Tp&>>
		 > = false>
        explicit
	constexpr
	optional(optional<G_Up>& g_rhs)
	noexcept(std::is_nothrow_constructible<G_Tp&, G_Up&>::value)
	{
	  if (g_rhs)
	    g_convert_ref_init_val(g_rhs.m_fwd());
	}
      template<typename G_Up,
	       G_Requires<
		 not_<std::is_same<std::remove_cv_t<G_Tp>, optional<G_Up>>>,
		 not_<std::is_same<G_Tp&, G_Up>>,
		 std::is_constructible<G_Tp&, G_Up&>,
		 not_<reference_constructs_from_temporary<G_Tp&, G_Up&>>,

		 // !Explicit:
		 std::is_convertible<G_Up&, G_Tp&>
		 > = false>
	constexpr
	optional(optional<G_Up>& g_rhs)
	noexcept(std::is_nothrow_constructible<G_Tp&, G_Up&>::value)
	{
	  if (g_rhs)
	    g_convert_ref_init_val(g_rhs.m_fwd());
	}

      template<typename G_Up,
	       G_Requires<
		 not_<std::is_same<std::remove_cv_t<G_Tp>, optional<G_Up>>>,
		 not_<std::is_same<G_Tp&, G_Up>>,
		 std::is_constructible<G_Tp&, G_Up&>,
		 reference_constructs_from_temporary<G_Tp&, G_Up&>,

		 // Explicit:
		 not_<std::is_convertible<G_Up&, G_Tp&>>
		 > = false>
        explicit
	constexpr
	optional(optional<G_Up>& g_rhs) = delete;
      template<typename G_Up,
	       G_Requires<
		 not_<std::is_same<std::remove_cv_t<G_Tp>, optional<G_Up>>>,
		 not_<std::is_same<G_Tp&, G_Up>>,
		 std::is_constructible<G_Tp&, G_Up&>,
		 reference_constructs_from_temporary<G_Tp&, G_Up&>,

		 // !Explicit:
		 std::is_convertible<G_Up&, G_Tp&>
		 > = false>
	constexpr
	optional(optional<G_Up>& g_rhs) = delete;

      // const optional<U>&
      template<typename G_Up,
	       G_Requires<
		 not_<std::is_same<std::remove_cv_t<G_Tp>, optional<G_Up>>>,
		 not_<std::is_same<G_Tp&, G_Up>>,
		 std::is_constructible<G_Tp&, const G_Up&>,
		 not_<reference_constructs_from_temporary<G_Tp&, const G_Up&>>,

		 // Explicit:
		 not_<std::is_convertible<const G_Up&, G_Tp&>>
		 > = false>
	explicit
	constexpr
	optional(const optional<G_Up>& g_rhs)
	noexcept(std::is_nothrow_constructible<G_Tp&, G_Up&>::value)
	{
	  if (g_rhs)
	    g_convert_ref_init_val(g_rhs.m_fwd());
	}
      template<typename G_Up,
	       G_Requires<
		 not_<std::is_same<std::remove_cv_t<G_Tp>, optional<G_Up>>>,
		 not_<std::is_same<G_Tp&, G_Up>>,
		 std::is_constructible<G_Tp&, const G_Up&>,
		 not_<reference_constructs_from_temporary<G_Tp&, const G_Up&>>,

		 // !Explicit:
		 std::is_convertible<const G_Up&, G_Tp&>
		 > = false>
	constexpr
	optional(const optional<G_Up>& g_rhs)
	noexcept(std::is_nothrow_constructible<G_Tp&, G_Up&>::value)
	{
	  if (g_rhs)
	    g_convert_ref_init_val(g_rhs.m_fwd());
	}

      template<typename G_Up,
	       G_Requires<
		 not_<std::is_same<std::remove_cv_t<G_Tp>, optional<G_Up>>>,
		 not_<std::is_same<G_Tp&, G_Up>>,
		 std::is_constructible<G_Tp&, const G_Up&>,
		 reference_constructs_from_temporary<G_Tp&, const G_Up&>,

		 // Explicit:
		 not_<std::is_convertible<const G_Up&, G_Tp&>>
		 > = false>
	explicit
	constexpr
	optional(const optional<G_Up>& g_rhs) = delete;
      template<typename G_Up,
	       G_Requires<
		 not_<std::is_same<std::remove_cv_t<G_Tp>, optional<G_Up>>>,
		 not_<std::is_same<G_Tp&, G_Up>>,
		 std::is_constructible<G_Tp&, const G_Up&>,
		 reference_constructs_from_temporary<G_Tp&, const G_Up&>,

		 // !Explicit:
		 std::is_convertible<const G_Up&, G_Tp&>
		 > = false>
	constexpr
	optional(const optional<G_Up>& g_rhs) = delete;

      // optional<U>&&
      template<typename G_Up,
	       G_Requires<
		 not_<std::is_same<std::remove_cv_t<G_Tp>, optional<G_Up>>>,
		 not_<std::is_same<G_Tp&, G_Up>>,
		 std::is_constructible<G_Tp&, G_Up>,
		 not_<reference_constructs_from_temporary<G_Tp&, G_Up>>,

		 // Explicit:
		 not_<std::is_convertible<G_Up, G_Tp&>>
		 > = false>
	explicit
	constexpr
	optional(optional<G_Up>&& g_rhs)
	noexcept(std::is_nothrow_constructible<G_Tp&, G_Up>::value)
	{
	  if (g_rhs)
	    g_convert_ref_init_val(std::move(g_rhs).m_fwd());
	}
      template<typename G_Up,
	       G_Requires<
		 not_<std::is_same<std::remove_cv_t<G_Tp>, optional<G_Up>>>,
		 not_<std::is_same<G_Tp&, G_Up>>,
		 std::is_constructible<G_Tp&, G_Up>,
		 not_<reference_constructs_from_temporary<G_Tp&, G_Up>>,

		 // !Explicit:
		 std::is_convertible<G_Up, G_Tp&>
		 > = false>
	constexpr
	optional(optional<G_Up>&& g_rhs)
	noexcept(std::is_nothrow_constructible<G_Tp&, G_Up>::value)
	{
	  if (g_rhs)
	    g_convert_ref_init_val(std::move(g_rhs).m_fwd());
	}

      template<typename G_Up,
	       G_Requires<
		 not_<std::is_same<std::remove_cv_t<G_Tp>, optional<G_Up>>>,
		 not_<std::is_same<G_Tp&, G_Up>>,
		 std::is_constructible<G_Tp&, G_Up>,
		 reference_constructs_from_temporary<G_Tp&, G_Up>,

		 // Explicit:
		 not_<std::is_convertible<G_Up, G_Tp&>>
		 > = false>
	explicit
	constexpr
	optional(optional<G_Up>&& g_rhs) = delete;
      template<typename G_Up,
	       G_Requires<
		 not_<std::is_same<std::remove_cv_t<G_Tp>, optional<G_Up>>>,
		 not_<std::is_same<G_Tp&, G_Up>>,
		 std::is_constructible<G_Tp&, G_Up>,
		 reference_constructs_from_temporary<G_Tp&, G_Up>,

		 // !Explicit:
		 std::is_convertible<G_Up, G_Tp&>
		 > = false>
	constexpr
	optional(optional<G_Up>&& g_rhs) = delete;

      // const optional<U>&&
      template<typename G_Up,
	       G_Requires<
		 not_<std::is_same<std::remove_cv_t<G_Tp>, optional<G_Up>>>,
		 not_<std::is_same<G_Tp&, G_Up>>,
		 std::is_constructible<G_Tp&, const G_Up>,
		 not_<reference_constructs_from_temporary<G_Tp&, G_Up>>,

		 // Explicit:
		 not_<std::is_convertible<const G_Up, G_Tp&>>
		 > = false>
	explicit
	constexpr
	optional(const optional<G_Up>&& g_rhs)
	noexcept(std::is_nothrow_constructible<G_Tp&, const G_Up>::value)
	{
	  if (g_rhs)
	    g_convert_ref_init_val(std::move(g_rhs).m_fwd());
	}
      template<typename G_Up,
	       G_Requires<
		 not_<std::is_same<std::remove_cv_t<G_Tp>, optional<G_Up>>>,
		 not_<std::is_same<G_Tp&, G_Up>>,
		 std::is_constructible<G_Tp&, const G_Up>,
		 not_<reference_constructs_from_temporary<G_Tp&, G_Up>>,

		 // !Explicit:
		 std::is_convertible<const G_Up, G_Tp&>
		 > = false>
	constexpr
	optional(const optional<G_Up>&& g_rhs)
	noexcept(std::is_nothrow_constructible<G_Tp&, const G_Up>::value)
	{
	  if (g_rhs)
	    g_convert_ref_init_val(std::move(g_rhs).m_fwd());
	}

      template<typename G_Up,
	       G_Requires<
		 not_<std::is_same<std::remove_cv_t<G_Tp>, optional<G_Up>>>,
		 not_<std::is_same<G_Tp&, G_Up>>,
		 std::is_constructible<G_Tp&, const G_Up>,
		 reference_constructs_from_temporary<G_Tp&, const G_Up>,

		 // Explicit:
		 not_<std::is_convertible<const G_Up, G_Tp&>>
		 > = false>
	explicit
	constexpr
	optional(const optional<G_Up>&& g_rhs) = delete;
      template<typename G_Up,
	       G_Requires<
		 not_<std::is_same<std::remove_cv_t<G_Tp>, optional<G_Up>>>,
		 not_<std::is_same<G_Tp&, G_Up>>,
		 std::is_constructible<G_Tp&, const G_Up>,
		 reference_constructs_from_temporary<G_Tp&, const G_Up>,

		 // !Explicit:
		 std::is_convertible<const G_Up, G_Tp&>
		 > = false>
	constexpr
	optional(const optional<G_Up>&& g_rhs) = delete;

      GCC_BACKPORT20_CONSTEXPR ~optional() = default;

      // Assignment.
      constexpr optional& operator=(nullopt_t) noexcept
      {
	m_val = nullptr;
	return *this;
      }

      constexpr optional& operator=(const optional&) noexcept = default;

      template<typename G_Up,
	       G_Requires<
		 std::is_constructible<G_Tp&, G_Up>,
		 not_<reference_constructs_from_temporary<G_Tp&, G_Up>>
		 > = false>
	constexpr G_Tp&
	emplace(G_Up&& g_u)
	noexcept(std::is_nothrow_constructible<G_Tp&, G_Up>::value)
	{
	  g_convert_ref_init_val(std::forward<G_Up>(g_u));
	  // _GLIBCXX_RESOLVE_LIB_DEFECTS
	  // 4300. Missing Returns: element in optional<T&>::emplace
	  return *m_val;
	}

      // Swap.
      constexpr void swap(optional& g_rhs) noexcept
      { std::swap(m_val, g_rhs.m_val); }

      // Observers.
      constexpr G_Tp* operator->() const noexcept
      {
	__glibcxx_assert(m_val); // hardened precondition
	return m_val;
      }

      constexpr G_Tp& operator*() const noexcept
      {
	__glibcxx_assert(m_val); // hardened precondition
	return *m_val;
      }

      constexpr explicit operator bool() const noexcept
      {
	return m_val;
      }

      constexpr bool has_value() const noexcept
      {
	return m_val;
      }

      constexpr G_Tp& value() const;

      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 4304. std::optional<NonReturnable&> is ill-formed due to value_or
      template<typename G_Up = std::remove_cv_t<G_Tp>,
	       typename G_Tp_ = G_Tp,
	       G_Requires<
		 std::is_object<G_Tp_>,
		 not_<std::is_array<G_Tp_>>
		 > = false>
	constexpr std::decay_t<G_Tp>
	value_or(G_Up&& g_u) const;

      // Monadic operations.
      template<typename G_Fn>
	constexpr auto
	and_then(G_Fn&& g_f) const;

      template<typename G_Fn>
	constexpr
	optional<std::remove_cv_t<g_invoke_result_t<G_Fn, G_Tp&>>>
	transform(G_Fn&& g_f) const;

      template<typename G_Fn,
	       G_Requires<g_is_invocable<G_Fn>> = false>
	constexpr
	optional
	or_else(G_Fn&& g_f) const;

      // Modifiers.
      constexpr void reset() noexcept
      {
	m_val = nullptr;
      }

    private:
      G_Tp *m_val = nullptr;

      [[__gnu__::__always_inline__]]
      constexpr G_Tp&
      m_fwd() const noexcept
      { return *m_val; }

      template<typename G_Up> friend class optional;

      template<typename G_Up>
	constexpr
	void
	g_convert_ref_init_val(G_Up&& g_u)
	noexcept
	{
	  G_Tp& g_r(std::forward<G_Up>(g_u));
	  m_val = std::addressof(g_r);
	}

      template<typename G_Fn, typename G_Value>
	explicit constexpr
	optional(G_Optional_func<G_Fn> g_f, G_Value&& g_v);
    };
} // namespace gcc

#endif // GCC_BACKPORT_GLIBCXX_OPTIONAL_REF

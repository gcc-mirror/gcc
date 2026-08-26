// A few more utilities used in the backports not corresponding to existing headers -*- C++ -*-

// Copyright (C) 2026 Free Software Foundation, Inc.
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

#ifndef GCC_BACKPORT_BITS_CONDITIONALS_H
#define GCC_BACKPORT_BITS_CONDITIONALS_H 1

#include <type_traits>
#include <cstddef>

#ifndef __has_builtin
# define gcc_backport_has_builtin(x) 0
#else
# define gcc_backport_has_builtin(x) __has_builtin (x)
#endif

// Helper.  Expands to 'constexpr' in C++20 and an empty string otherwise.
#if __cplusplus >= 202002L
# define GCC_BACKPORT20_CONSTEXPR constexpr
#else
# define GCC_BACKPORT20_CONSTEXPR
#endif

namespace gcc
{
  template<typename G_Tp>
    [[nodiscard]]
    constexpr std::add_const_t<G_Tp>&
    as_const(G_Tp& g_t) noexcept
    { return g_t; }

  // std::void_t in C++14.
  template<typename...>
    using void_t = void;

  // type_identity in C++14.
  template <typename Type>
    struct type_identity
    { using type = Type; };

  template<typename Tp>
    using type_identity_t = typename type_identity<Tp>::type;

  // remove_cvref_t (std::remove_cvref_t for C++11).
  template<typename Tp>
    using remove_cvref_t
      = typename std::remove_cv<typename std::remove_reference<Tp>::type>::type;

  // std::bool_constant in C++14.
  template<bool B>
  using bool_constant = std::integral_constant<bool, B>;

  // Conditional helpers.

  namespace detail
  {
    // A variadic alias template that resolves to its first argument.
    template<typename G_Tp, typename...>
      using first_t = G_Tp;

    // These are deliberately not defined.
    template<typename... G_Bn>
      auto or_fn(int) -> first_t<std::false_type,
				 std::enable_if_t<!bool(G_Bn::value)>...>;

    template<typename... G_Bn>
      auto or_fn(...) -> std::true_type;

    template<typename... G_Bn>
      auto and_fn(int) -> first_t<std::true_type,
				  std::enable_if_t<bool(G_Bn::value)>...>;

    template<typename... G_Bn>
      auto and_fn(...) -> std::false_type;
  } // namespace detail

  // Like C++17 std::dis/conjunction, but usable in C++11 and resolves
  // to either true_type or false_type which allows for a more efficient
  // implementation that avoids recursive class template instantiation.
  template<typename... G_Bn>
    struct or_
    : decltype(detail::or_fn<G_Bn...>(0))
    { };

  template<typename... G_Bn>
    struct and_
    : decltype(detail::and_fn<G_Bn...>(0))
    { };

  template<typename G_Pp>
    struct not_
    : bool_constant<!bool(G_Pp::value)>
    { };

  namespace g_swappable_details
  {
    using std::swap;

    struct g_do_is_swappable_impl
    {
      template<typename G_Tp, typename
               = decltype(swap(std::declval<G_Tp&>(), std::declval<G_Tp&>()))>
        static std::true_type g_test(int);

      template<typename>
        static std::false_type g_test(...);
    };

    struct g_do_is_nothrow_swappable_impl
    {
      template<typename G_Tp>
        static bool_constant<
          noexcept(swap(std::declval<G_Tp&>(), std::declval<G_Tp&>()))
        > g_test(int);

      template<typename>
        static std::false_type g_test(...);
    };

  } // namespace g_swappable_details

  template<typename G_Tp>
    struct g_is_swappable_impl
    : public g_swappable_details::g_do_is_swappable_impl
    {
      using type = decltype(g_test<G_Tp>(0));
    };

  template<typename G_Tp>
    struct g_is_nothrow_swappable_impl
    : public g_swappable_details::g_do_is_nothrow_swappable_impl
    {
      using type = decltype(g_test<G_Tp>(0));
    };

  template<typename G_Tp>
    struct g_is_swappable
    : public g_is_swappable_impl<G_Tp>::type
    { };

  template<typename G_Tp>
    struct g_is_nothrow_swappable
    : public g_is_nothrow_swappable_impl<G_Tp>::type
    { };

  // Used by INVOKE.
  template<typename _Tp>
    struct g_success_type
    { using type = _Tp; };

  struct g_failure_type
  { };

  // Sfinae-friendly result_of implementation:

  struct g_invoke_memfun_ref { };
  struct g_invoke_memfun_deref { };
  struct g_invoke_memobj_ref { };
  struct g_invoke_memobj_deref { };
  struct g_invoke_other { };

  // Associate a tag type with a specialization of __success_type.
  template<typename G_Tp, typename G_Tag>
    struct g_result_of_success : g_success_type<G_Tp>
    { using g_invoke_type = G_Tag; };

  // [func.require] paragraph 1 bullet 1:
  struct g_result_of_memfun_ref_impl
  {
    template<typename G_Fp, typename G_Tp1, typename... G_Args>
      static g_result_of_success<decltype(
      (std::declval<G_Tp1>().*std::declval<G_Fp>())(std::declval<G_Args>()...)
      ), g_invoke_memfun_ref> G_S_test(int);

    template<typename...>
      static g_failure_type G_S_test(...);
  };

  template<typename G_MemPtr, typename G_Arg, typename... G_Args>
    struct g_result_of_memfun_ref
    : private g_result_of_memfun_ref_impl
    {
      using type = decltype(G_S_test<G_MemPtr, G_Arg, G_Args...>(0));
    };

  // [func.require] paragraph 1 bullet 2:
  struct g_result_of_memfun_deref_impl
  {
    template<typename G_Fp, typename G_Tp1, typename... G_Args>
      static g_result_of_success<decltype(
      ((*std::declval<G_Tp1>()).*std::declval<G_Fp>())(std::declval<G_Args>()...)
      ), g_invoke_memfun_deref> G_S_test(int);

    template<typename...>
      static g_failure_type G_S_test(...);
  };

  template<typename G_MemPtr, typename G_Arg, typename... G_Args>
    struct g_result_of_memfun_deref
    : private g_result_of_memfun_deref_impl
    {
      using type = decltype(G_S_test<G_MemPtr, G_Arg, G_Args...>(0));
    };

  // [func.require] paragraph 1 bullet 3:
  struct g_result_of_memobj_ref_impl
  {
    template<typename G_Fp, typename G_Tp1>
      static g_result_of_success<decltype(
      std::declval<G_Tp1>().*std::declval<G_Fp>()
      ), g_invoke_memobj_ref> G_S_test(int);

    template<typename, typename>
      static g_failure_type G_S_test(...);
  };

  template<typename G_MemPtr, typename G_Arg>
    struct g_result_of_memobj_ref
    : private g_result_of_memobj_ref_impl
    {
      using type = decltype(G_S_test<G_MemPtr, G_Arg>(0));
    };

  // [func.require] paragraph 1 bullet 4:
  struct g_result_of_memobj_deref_impl
  {
    template<typename G_Fp, typename G_Tp1>
      static g_result_of_success<decltype(
      (*std::declval<G_Tp1>()).*std::declval<G_Fp>()
      ), g_invoke_memobj_deref> G_S_test(int);

    template<typename, typename>
      static g_failure_type G_S_test(...);
  };

  template<typename G_MemPtr, typename G_Arg>
    struct g_result_of_memobj_deref
    : private g_result_of_memobj_deref_impl
    {
      using type = decltype(G_S_test<G_MemPtr, G_Arg>(0));
    };

  template<typename G_MemPtr, typename G_Arg>
    struct g_result_of_memobj;

  template<typename G_Res, typename G_Class, typename G_Arg>
    struct g_result_of_memobj<G_Res G_Class::*, G_Arg>
    {
      using G_Argval = remove_cvref_t<G_Arg>;
      using G_MemPtr = G_Res G_Class::*;
      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 3655. The INVOKE operation and union types
      using type = typename std::conditional_t<or_<std::is_same<G_Argval, G_Class>,
        std::is_base_of<G_Class, G_Argval>>::value,
        g_result_of_memobj_ref<G_MemPtr, G_Arg>,
        g_result_of_memobj_deref<G_MemPtr, G_Arg>
      >::type;
    };

  template<typename G_MemPtr, typename G_Arg, typename... G_Args>
    struct g_result_of_memfun;

  template<typename G_Res, typename G_Class, typename G_Arg, typename... G_Args>
    struct g_result_of_memfun<G_Res G_Class::*, G_Arg, G_Args...>
    {
      using G_Argval = typename std::remove_reference<G_Arg>::type;
      using G_MemPtr = G_Res G_Class::*;
      using type = typename std::conditional_t<std::is_base_of<G_Class, G_Argval>::value,
        g_result_of_memfun_ref<G_MemPtr, G_Arg, G_Args...>,
        g_result_of_memfun_deref<G_MemPtr, G_Arg, G_Args...>
      >::type;
    };

  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 2219.  INVOKE-ing a pointer to member with a reference_wrapper
  //        as the object expression

  // Used by result_of, invoke etc. to unwrap a reference_wrapper.
  template<typename G_Tp, typename G_Up = remove_cvref_t<G_Tp>>
    struct g_inv_unwrap
    {
      using type = G_Tp;
    };

  template<typename G_Tp, typename G_Up>
    struct g_inv_unwrap<G_Tp, std::reference_wrapper<G_Up>>
    {
      using type = G_Up&;
    };

  template<bool, bool, typename G_Functor, typename... G_ArgTypes>
    struct g_result_of_impl
    {
      using type = g_failure_type;
    };

  template<typename G_MemPtr, typename G_Arg>
    struct g_result_of_impl<true, false, G_MemPtr, G_Arg>
    : public g_result_of_memobj<std::decay_t<G_MemPtr>,
				typename g_inv_unwrap<G_Arg>::type>
    { };

  template<typename G_MemPtr, typename G_Arg, typename... G_Args>
    struct g_result_of_impl<false, true, G_MemPtr, G_Arg, G_Args...>
    : public g_result_of_memfun<std::decay_t<G_MemPtr>,
				typename g_inv_unwrap<G_Arg>::type, G_Args...>
    { };

  // [func.require] paragraph 1 bullet 5:
  struct g_result_of_other_impl
  {
    template<typename G_Fn, typename... G_Args>
      static g_result_of_success<decltype(
      std::declval<G_Fn>()(std::declval<G_Args>()...)
      ), g_invoke_other> G_S_test(int);

    template<typename...>
      static g_failure_type G_S_test(...);
  };

  template<typename G_Functor, typename... G_ArgTypes>
    struct g_result_of_impl<false, false, G_Functor, G_ArgTypes...>
    : private g_result_of_other_impl
    {
      using type = decltype(G_S_test<G_Functor, G_ArgTypes...>(0));
    };

  // __invoke_result (std::invoke_result for C++11)
  template<typename G_Functor, typename... G_ArgTypes>
    struct g_invoke_result
    : public g_result_of_impl<
        std::is_member_object_pointer<
          typename std::remove_reference<G_Functor>::type
        >::value,
        std::is_member_function_pointer<
          typename std::remove_reference<G_Functor>::type
        >::value,
	G_Functor, G_ArgTypes...
      >::type
    { };

  // __invoke_result_t (std::invoke_result_t for C++11)
  template<typename G_Fn, typename... G_Args>
    using g_invoke_result_t = typename g_invoke_result<G_Fn, G_Args...>::type;

  // __is_invocable (std::is_invocable for C++11)

  // The primary template is used for invalid INVOKE expressions.
  template<typename G_Result, typename G_Ret,
	   bool = std::is_void<G_Ret>::value, typename = void>
    struct g_is_invocable_impl
    : std::false_type
    {
      using g_nothrow_conv = std::false_type; // For is_nothrow_invocable_r
    };

  // Used for valid INVOKE and INVOKE<void> expressions.
  template<typename G_Result, typename G_Ret>
    struct g_is_invocable_impl<G_Result, G_Ret,
			       /* is_void<_Ret> = */ true,
			       void_t<typename G_Result::type>>
    : std::true_type
    {
      using g_nothrow_conv = std::true_type; // For is_nothrow_invocable_r
    };

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wctor-dtor-privacy"
  // Used for INVOKE<R> expressions to check the implicit conversion to R.
  template<typename G_Result, typename G_Ret>
    struct g_is_invocable_impl<G_Result, G_Ret,
			       /* is_void<_Ret> = */ false,
			       void_t<typename G_Result::type>>
    {
    private:
      // The type of the INVOKE expression.
      using G_Res_t = typename G_Result::type;

      // Unlike declval, this doesn't add_rvalue_reference, so it respects
      // guaranteed copy elision.
      static G_Res_t G_S_get() noexcept;

      // Used to check if _Res_t can implicitly convert to _Tp.
      template<typename G_Tp>
	static void G_S_conv(type_identity_t<G_Tp>) noexcept;

      // This overload is viable if INVOKE(f, args...) can convert to _Tp.
      template<typename G_Tp,
	       bool G_Nothrow = noexcept(G_S_conv<G_Tp>(G_S_get())),
	       typename = decltype(G_S_conv<G_Tp>(G_S_get())),
#if gcc_backport_has_builtin(__reference_converts_from_temporary)
	       bool G_Dangle = __reference_converts_from_temporary(G_Tp, G_Res_t)
#else
	       bool G_Dangle = false
#endif
	      >
	static bool_constant<G_Nothrow && !G_Dangle>
	G_S_test(int);

      template<typename G_Tp, bool = false>
	static std::false_type
	G_S_test(...);

    public:
      // For is_invocable_r
      using type = decltype(G_S_test<G_Ret, /* Nothrow = */ true>(1));

      // For is_nothrow_invocable_r
      using g_nothrow_conv = decltype(G_S_test<G_Ret>(1));
    };
#pragma GCC diagnostic pop

  template<typename G_Fn, typename... G_ArgTypes>
    struct g_is_invocable
    : g_is_invocable_impl<g_invoke_result<G_Fn, G_ArgTypes...>, void>::type
    { };

  template<typename G_Fn, typename G_Tp, typename... G_Args>
    constexpr bool g_call_is_nt(g_invoke_memfun_ref)
    {
      using G_Up = typename g_inv_unwrap<G_Tp>::type;
      return noexcept((std::declval<G_Up>().*std::declval<G_Fn>())(
	    std::declval<G_Args>()...));
    }

  template<typename G_Fn, typename G_Tp, typename... G_Args>
    constexpr bool g_call_is_nt(g_invoke_memfun_deref)
    {
      return noexcept(((*std::declval<G_Tp>()).*std::declval<G_Fn>())(
	    std::declval<G_Args>()...));
    }

  template<typename G_Fn, typename G_Tp>
    constexpr bool g_call_is_nt(g_invoke_memobj_ref)
    {
      using G_Up = typename g_inv_unwrap<G_Tp>::type;
      return noexcept(std::declval<G_Up>().*std::declval<G_Fn>());
    }

  template<typename G_Fn, typename G_Tp>
    constexpr bool g_call_is_nt(g_invoke_memobj_deref)
    {
      return noexcept((*std::declval<G_Tp>()).*std::declval<G_Fn>());
    }

  template<typename G_Fn, typename... G_Args>
    constexpr bool g_call_is_nt(g_invoke_other)
    {
      return noexcept(std::declval<G_Fn>()(std::declval<G_Args>()...));
    }

  template<typename G_Result, typename G_Fn, typename... G_Args>
    struct g_call_is_nothrow
    : bool_constant<
	g_call_is_nt<G_Fn, G_Args...>(typename G_Result::g_invoke_type{})
      >
    { };

  template<typename G_Fn, typename... G_Args>
    using g_call_is_nothrow_
      = g_call_is_nothrow<g_invoke_result<G_Fn, G_Args...>, G_Fn, G_Args...>;

  // __is_nothrow_invocable (std::is_nothrow_invocable for C++11)
  template<typename G_Fn, typename... G_Args>
    struct g_is_nothrow_invocable
    : and_<g_is_invocable<G_Fn, G_Args...>,
             g_call_is_nothrow_<G_Fn, G_Args...>>::type
    { };

  // std::is_unbounded_array backport
  template<typename G_Tp>
    struct is_unbounded_array : std::false_type {};

  template<typename G_Tp>
    struct is_unbounded_array<G_Tp[]> : std::true_type {};

  // true_type if G_Tp has cv-quals.
  template<typename G_Tp>
    using is_cv_qualified = not_<std::is_same<G_Tp, std::remove_cv_t<G_Tp>>>;

  // Helpers for relational operators.  Trick borrowed from <optional>.
  namespace detail {
    template<typename... G_Tp>
      using relop_return_t = std::enable_if_t<
	and_<std::is_convertible<G_Tp, bool>...>::value,
	bool
	>;
  }

  // 'bool' if G_Tp has equality operator, otherwise SFINAE fail.
  template<typename G_Tp, typename G_Up = G_Tp>
    using eql_return_t = detail::relop_return_t<
      decltype(std::declval<const G_Tp&>() == std::declval<const G_Up&>())
      >;


  template<typename G_Tp, typename G_Wp>
    using g_converts_from_any_cvref = or_<
	std::is_constructible<G_Tp, G_Wp&>,       std::is_convertible<G_Wp&, G_Tp>,
	std::is_constructible<G_Tp, G_Wp>,        std::is_convertible<G_Wp, G_Tp>,
	std::is_constructible<G_Tp, const G_Wp&>, std::is_convertible<const G_Wp&, G_Tp>,
	std::is_constructible<G_Tp, const G_Wp>,  std::is_convertible<const G_Wp, G_Tp>
      >;

  template<typename... G_Cond>
    using G_Requires = std::enable_if_t<and_<G_Cond...>::value, bool>;


  template<typename G_Tp>
    struct g_is_array_known_bounds
    : public std::false_type
    { };

  template<typename G_Tp, std::size_t G_Size>
    struct g_is_array_known_bounds<G_Tp[G_Size]>
    : public std::true_type
    { };

  template<typename G_Tp>
    struct g_is_array_unknown_bounds
    : public std::false_type
    { };

  template<typename G_Tp>
    struct g_is_array_unknown_bounds<G_Tp[]>
    : public std::true_type
    { };

  // An object type which is not an unbounded array.
  // It might still be an incomplete type, but if this is false_type
  // then we can be certain it's not a complete object type.
  template<typename G_Tp>
    using g_maybe_complete_object_type
      = and_<std::is_object<G_Tp>, not_<g_is_array_unknown_bounds<G_Tp>>>;

  // Helper functions that return false_type for incomplete classes,
  // incomplete unions and arrays of known bound from those.

  // More specialized overload for complete object types (returning true_type).
  template<typename G_Tp,
	   typename = std::enable_if_t<g_maybe_complete_object_type<G_Tp>::value>,
	   std::size_t = sizeof(G_Tp)>
    constexpr std::true_type
    g_is_complete_or_unbounded(type_identity<G_Tp>)
    { return {}; };

  // Less specialized overload for reference and unknown-bound array types
  // (returning true_type), and incomplete types (returning false_type).
  template<typename G_TypeIdentity,
	   typename G_NestedType = typename G_TypeIdentity::type>
    constexpr typename not_<g_maybe_complete_object_type<G_NestedType>>::type
    g_is_complete_or_unbounded(G_TypeIdentity)
    { return {}; }


#if gcc_backport_has_builtin(__reference_converts_from_temporary)
  /// True if _Tp is a reference type, a _Up value can be bound to _Tp in
  /// copy-initialization, and a temporary object would be bound to
  /// the reference, false otherwise.
  /// @since C++23
  template<typename _Tp, typename _Up>
    struct reference_converts_from_temporary
    : public bool_constant<__reference_converts_from_temporary(_Tp, _Up)>
    {
      static_assert(g_is_complete_or_unbounded(type_identity<_Tp>{})
		    && g_is_complete_or_unbounded(type_identity<_Up>{}),
	"template argument must be a complete class or an unbounded array");
    };
#else
  // Hopefully, this trait is only ever used to enable developer diagnostics,
  // and doesn't impact correctness.
  template<typename _Tp, typename _Up>
    struct reference_converts_from_temporary : std::false_type {};
#endif

#if gcc_backport_has_builtin(__reference_converts_from_temporary)
  /// True if _Tp is a reference type, a _Up value can be bound to _Tp in
  /// direct-initialization, and a temporary object would be bound to
  /// the reference, false otherwise.
  /// @since C++23
  template<typename G_Tp, typename G_Up>
    struct reference_constructs_from_temporary
    : public bool_constant<__reference_constructs_from_temporary(G_Tp, G_Up)>
    {
      static_assert(g_is_complete_or_unbounded(type_identity<G_Tp>{})
		    && g_is_complete_or_unbounded(type_identity<G_Up>{}),
	"template argument must be a complete class or an unbounded array");
    };
#else
  // Hopefully, this trait is only ever used to enable developer diagnostics,
  // and doesn't impact correctness.
  template<typename _Tp, typename _Up>
    struct reference_constructs_from_temporary : std::false_type {};
#endif
} // namespace gcc

#endif // GCC_BACKPORT_BITS_CONDITIONALS_H

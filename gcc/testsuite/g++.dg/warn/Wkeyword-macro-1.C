// C++26 P2843R3 - Preprocessing is never undefined
// [cpp.replace.general]/9
// { dg-do preprocess }
// { dg-options "-pedantic-errors" }
// { dg-additional-options "-fmodules" { target c++20 } }

// [lex.key]
#define alignas 1			// { dg-error "keyword 'alignas' defined as macro" "" { target c++26 } }
#define alignof 1			// { dg-error "keyword 'alignof' defined as macro" "" { target c++26 } }
#define asm 1				// { dg-error "keyword 'asm' defined as macro" "" { target c++26 } }
#define auto 1				// { dg-error "keyword 'auto' defined as macro" "" { target c++26 } }
#define bool 1				// { dg-error "keyword 'bool' defined as macro" "" { target c++26 } }
#define break 1				// { dg-error "keyword 'break' defined as macro" "" { target c++26 } }
#define case 1				// { dg-error "keyword 'case' defined as macro" "" { target c++26 } }
#define catch 1				// { dg-error "keyword 'catch' defined as macro" "" { target c++26 } }
#define char 1				// { dg-error "keyword 'char' defined as macro" "" { target c++26 } }
#define char16_t 1			// { dg-error "keyword 'char16_t' defined as macro" "" { target c++26 } }
#define char32_t 1			// { dg-error "keyword 'char32_t' defined as macro" "" { target c++26 } }
#define char8_t 1			// { dg-error "keyword 'char8_t' defined as macro" "" { target c++26 } }
#define class 1				// { dg-error "keyword 'class' defined as macro" "" { target c++26 } }
#define co_await 1			// { dg-error "keyword 'co_await' defined as macro" "" { target c++26 } }
#define concept 1			// { dg-error "keyword 'concept' defined as macro" "" { target c++26 } }
#define const 1				// { dg-error "keyword 'const' defined as macro" "" { target c++26 } }
#define const_cast 1			// { dg-error "keyword 'const_cast' defined as macro" "" { target c++26 } }
#define consteval 1			// { dg-error "keyword 'consteval' defined as macro" "" { target c++26 } }
#define constexpr 1			// { dg-error "keyword 'constexpr' defined as macro" "" { target c++26 } }
#define constinit 1			// { dg-error "keyword 'constinit' defined as macro" "" { target c++26 } }
#define continue 1			// { dg-error "keyword 'continue' defined as macro" "" { target c++26 } }
#define contract_assert 1
#define co_return 1			// { dg-error "keyword 'co_return' defined as macro" "" { target c++26 } }
#define co_yield 1			// { dg-error "keyword 'co_yield' defined as macro" "" { target c++26 } }
#define decltype 1			// { dg-error "keyword 'decltype' defined as macro" "" { target c++26 } }
#define default 1			// { dg-error "keyword 'default' defined as macro" "" { target c++26 } }
#define delete 1			// { dg-error "keyword 'delete' defined as macro" "" { target c++26 } }
#define do 1				// { dg-error "keyword 'do' defined as macro" "" { target c++26 } }
#define double 1			// { dg-error "keyword 'double' defined as macro" "" { target c++26 } }
#define dynamic_cast 1			// { dg-error "keyword 'dynamic_cast' defined as macro" "" { target c++26 } }
#define else 1				// { dg-error "keyword 'else' defined as macro" "" { target c++26 } }
#define enum 1				// { dg-error "keyword 'enum' defined as macro" "" { target c++26 } }
#define explicit 1			// { dg-error "keyword 'explicit' defined as macro" "" { target c++26 } }
#define export 1			// { dg-error "keyword 'export' defined as macro" "" { target c++26 } }
#define extern 1			// { dg-error "keyword 'extern' defined as macro" "" { target c++26 } }
#define false 1				// { dg-error "keyword 'false' defined as macro" "" { target c++26 } }
#define float 1				// { dg-error "keyword 'float' defined as macro" "" { target c++26 } }
#define for 1				// { dg-error "keyword 'for' defined as macro" "" { target c++26 } }
#define friend 1			// { dg-error "keyword 'friend' defined as macro" "" { target c++26 } }
#define goto 1				// { dg-error "keyword 'goto' defined as macro" "" { target c++26 } }
#define if 1				// { dg-error "keyword 'if' defined as macro" "" { target c++26 } }
#define inline 1			// { dg-error "keyword 'inline' defined as macro" "" { target c++26 } }
#define int 1				// { dg-error "keyword 'int' defined as macro" "" { target c++26 } }
#define long 1				// { dg-error "keyword 'long' defined as macro" "" { target c++26 } }
#define mutable 1			// { dg-error "keyword 'mutable' defined as macro" "" { target c++26 } }
#define namespace 1			// { dg-error "keyword 'namespace' defined as macro" "" { target c++26 } }
#define new 1				// { dg-error "keyword 'new' defined as macro" "" { target c++26 } }
#define noexcept 1			// { dg-error "keyword 'noexcept' defined as macro" "" { target c++26 } }
#define nullptr 1			// { dg-error "keyword 'nullptr' defined as macro" "" { target c++26 } }
#define operator 1			// { dg-error "keyword 'operator' defined as macro" "" { target c++26 } }
#define private 1			// { dg-error "keyword 'private' defined as macro" "" { target c++26 } }
#define protected 1			// { dg-error "keyword 'protected' defined as macro" "" { target c++26 } }
#define public 1			// { dg-error "keyword 'public' defined as macro" "" { target c++26 } }
#define register 1			// { dg-error "keyword 'register' defined as macro" "" { target c++26 } }
#define reinterpret_cast 1		// { dg-error "keyword 'reinterpret_cast' defined as macro" "" { target c++26 } }
#define requires 1			// { dg-error "keyword 'requires' defined as macro" "" { target c++26 } }
#define return 1			// { dg-error "keyword 'return' defined as macro" "" { target c++26 } }
#define short 1				// { dg-error "keyword 'short' defined as macro" "" { target c++26 } }
#define signed 1			// { dg-error "keyword 'signed' defined as macro" "" { target c++26 } }
#define sizeof 1			// { dg-error "keyword 'sizeof' defined as macro" "" { target c++26 } }
#define static 1			// { dg-error "keyword 'static' defined as macro" "" { target c++26 } }
#define static_assert 1			// { dg-error "keyword 'static_assert' defined as macro" "" { target c++26 } }
#define static_cast 1			// { dg-error "keyword 'static_cast' defined as macro" "" { target c++26 } }
#define struct 1			// { dg-error "keyword 'struct' defined as macro" "" { target c++26 } }
#define switch 1			// { dg-error "keyword 'switch' defined as macro" "" { target c++26 } }
#define template 1			// { dg-error "keyword 'template' defined as macro" "" { target c++26 } }
#define this 1				// { dg-error "keyword 'this' defined as macro" "" { target c++26 } }
#define thread_local 1			// { dg-error "keyword 'thread_local' defined as macro" "" { target c++26 } }
#define throw 1				// { dg-error "keyword 'throw' defined as macro" "" { target c++26 } }
#define true 1				// { dg-error "keyword 'true' defined as macro" "" { target c++26 } }
#define try 1				// { dg-error "keyword 'try' defined as macro" "" { target c++26 } }
#define typedef 1			// { dg-error "keyword 'typedef' defined as macro" "" { target c++26 } }
#define typeid 1			// { dg-error "keyword 'typeid' defined as macro" "" { target c++26 } }
#define typename 1			// { dg-error "keyword 'typename' defined as macro" "" { target c++26 } }
#define union 1				// { dg-error "keyword 'union' defined as macro" "" { target c++26 } }
#define unsigned 1			// { dg-error "keyword 'unsigned' defined as macro" "" { target c++26 } }
#define using 1				// { dg-error "keyword 'using' defined as macro" "" { target c++26 } }
#define virtual 1			// { dg-error "keyword 'virtual' defined as macro" "" { target c++26 } }
#define void 1				// { dg-error "keyword 'void' defined as macro" "" { target c++26 } }
#define volatile 1			// { dg-error "keyword 'volatile' defined as macro" "" { target c++26 } }
#define wchar_t 1			// { dg-error "keyword 'wchar_t' defined as macro" "" { target c++26 } }
#define while 1				// { dg-error "keyword 'while' defined as macro" "" { target c++26 } }

// [lex.name]
#define final 1				// { dg-error "keyword 'final' defined as macro" "" { target c++26 } }
#define import 1			// { dg-error "keyword 'import' defined as macro" "" { target c++26 } }
#define module 1			// { dg-error "keyword 'module' defined as macro" "" { target c++26 } }
#define override 1			// { dg-error "keyword 'override' defined as macro" "" { target c++26 } }
#define post 1
#define pre 1
#define replaceable_if_eligible
#define trivially_relocatable_if_eligible

// [dcl.attr]
#define assume 1			// { dg-error "keyword 'assume' defined as macro" "" { target c++26 } }
#define carries_dependency 1
#define deprecated 1			// { dg-error "keyword 'deprecated' defined as macro" "" { target c++26 } }
#define fallthrough 1			// { dg-error "keyword 'fallthrough' defined as macro" "" { target c++26 } }
#define indeterminate 1			// { dg-error "keyword 'indeterminate' defined as macro" "" { target c++26 } }
#define likely 1			// { dg-error "keyword 'likely' defined as macro" "" { target c++26 } }
#define maybe_unused 1			// { dg-error "keyword 'maybe_unused' defined as macro" "" { target c++26 } }
#define nodiscard 1			// { dg-error "keyword 'nodiscard' defined as macro" "" { target c++26 } }
#define noreturn 1			// { dg-error "keyword 'noreturn' defined as macro" "" { target c++26 } }
#define no_unique_address 1		// { dg-error "keyword 'no_unique_address' defined as macro" "" { target c++26 } }
#define unlikely 1			// { dg-error "keyword 'unlikely' defined as macro" "" { target c++26 } }

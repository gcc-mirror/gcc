// C++26 P2843R3 - Preprocessing is never undefined
// [cpp.replace.general]/9
// { dg-do preprocess }
// { dg-options "-pedantic" }
// { dg-additional-options "-fmodules" { target c++20 } }

// [lex.key]
#define alignas 1			// { dg-warning "keyword 'alignas' defined as macro" "" { target c++26 } }
#define alignof 1			// { dg-warning "keyword 'alignof' defined as macro" "" { target c++26 } }
#define asm 1				// { dg-warning "keyword 'asm' defined as macro" "" { target c++26 } }
#define auto 1				// { dg-warning "keyword 'auto' defined as macro" "" { target c++26 } }
#define bool 1				// { dg-warning "keyword 'bool' defined as macro" "" { target c++26 } }
#define break 1				// { dg-warning "keyword 'break' defined as macro" "" { target c++26 } }
#define case 1				// { dg-warning "keyword 'case' defined as macro" "" { target c++26 } }
#define catch 1				// { dg-warning "keyword 'catch' defined as macro" "" { target c++26 } }
#define char 1				// { dg-warning "keyword 'char' defined as macro" "" { target c++26 } }
#define char16_t 1			// { dg-warning "keyword 'char16_t' defined as macro" "" { target c++26 } }
#define char32_t 1			// { dg-warning "keyword 'char32_t' defined as macro" "" { target c++26 } }
#define char8_t 1			// { dg-warning "keyword 'char8_t' defined as macro" "" { target c++26 } }
#define class 1				// { dg-warning "keyword 'class' defined as macro" "" { target c++26 } }
#define co_await 1			// { dg-warning "keyword 'co_await' defined as macro" "" { target c++26 } }
#define concept 1			// { dg-warning "keyword 'concept' defined as macro" "" { target c++26 } }
#define const 1				// { dg-warning "keyword 'const' defined as macro" "" { target c++26 } }
#define const_cast 1			// { dg-warning "keyword 'const_cast' defined as macro" "" { target c++26 } }
#define consteval 1			// { dg-warning "keyword 'consteval' defined as macro" "" { target c++26 } }
#define constexpr 1			// { dg-warning "keyword 'constexpr' defined as macro" "" { target c++26 } }
#define constinit 1			// { dg-warning "keyword 'constinit' defined as macro" "" { target c++26 } }
#define continue 1			// { dg-warning "keyword 'continue' defined as macro" "" { target c++26 } }
#define contract_assert 1
#define co_return 1			// { dg-warning "keyword 'co_return' defined as macro" "" { target c++26 } }
#define co_yield 1			// { dg-warning "keyword 'co_yield' defined as macro" "" { target c++26 } }
#define decltype 1			// { dg-warning "keyword 'decltype' defined as macro" "" { target c++26 } }
#define default 1			// { dg-warning "keyword 'default' defined as macro" "" { target c++26 } }
#define delete 1			// { dg-warning "keyword 'delete' defined as macro" "" { target c++26 } }
#define do 1				// { dg-warning "keyword 'do' defined as macro" "" { target c++26 } }
#define double 1			// { dg-warning "keyword 'double' defined as macro" "" { target c++26 } }
#define dynamic_cast 1			// { dg-warning "keyword 'dynamic_cast' defined as macro" "" { target c++26 } }
#define else 1				// { dg-warning "keyword 'else' defined as macro" "" { target c++26 } }
#define enum 1				// { dg-warning "keyword 'enum' defined as macro" "" { target c++26 } }
#define explicit 1			// { dg-warning "keyword 'explicit' defined as macro" "" { target c++26 } }
#define export 1			// { dg-warning "keyword 'export' defined as macro" "" { target c++26 } }
#define extern 1			// { dg-warning "keyword 'extern' defined as macro" "" { target c++26 } }
#define false 1				// { dg-warning "keyword 'false' defined as macro" "" { target c++26 } }
#define float 1				// { dg-warning "keyword 'float' defined as macro" "" { target c++26 } }
#define for 1				// { dg-warning "keyword 'for' defined as macro" "" { target c++26 } }
#define friend 1			// { dg-warning "keyword 'friend' defined as macro" "" { target c++26 } }
#define goto 1				// { dg-warning "keyword 'goto' defined as macro" "" { target c++26 } }
#define if 1				// { dg-warning "keyword 'if' defined as macro" "" { target c++26 } }
#define inline 1			// { dg-warning "keyword 'inline' defined as macro" "" { target c++26 } }
#define int 1				// { dg-warning "keyword 'int' defined as macro" "" { target c++26 } }
#define long 1				// { dg-warning "keyword 'long' defined as macro" "" { target c++26 } }
#define mutable 1			// { dg-warning "keyword 'mutable' defined as macro" "" { target c++26 } }
#define namespace 1			// { dg-warning "keyword 'namespace' defined as macro" "" { target c++26 } }
#define new 1				// { dg-warning "keyword 'new' defined as macro" "" { target c++26 } }
#define noexcept 1			// { dg-warning "keyword 'noexcept' defined as macro" "" { target c++26 } }
#define nullptr 1			// { dg-warning "keyword 'nullptr' defined as macro" "" { target c++26 } }
#define operator 1			// { dg-warning "keyword 'operator' defined as macro" "" { target c++26 } }
#define private 1			// { dg-warning "keyword 'private' defined as macro" "" { target c++26 } }
#define protected 1			// { dg-warning "keyword 'protected' defined as macro" "" { target c++26 } }
#define public 1			// { dg-warning "keyword 'public' defined as macro" "" { target c++26 } }
#define register 1			// { dg-warning "keyword 'register' defined as macro" "" { target c++26 } }
#define reinterpret_cast 1		// { dg-warning "keyword 'reinterpret_cast' defined as macro" "" { target c++26 } }
#define requires 1			// { dg-warning "keyword 'requires' defined as macro" "" { target c++26 } }
#define return 1			// { dg-warning "keyword 'return' defined as macro" "" { target c++26 } }
#define short 1				// { dg-warning "keyword 'short' defined as macro" "" { target c++26 } }
#define signed 1			// { dg-warning "keyword 'signed' defined as macro" "" { target c++26 } }
#define sizeof 1			// { dg-warning "keyword 'sizeof' defined as macro" "" { target c++26 } }
#define static 1			// { dg-warning "keyword 'static' defined as macro" "" { target c++26 } }
#define static_assert 1			// { dg-warning "keyword 'static_assert' defined as macro" "" { target c++26 } }
#define static_cast 1			// { dg-warning "keyword 'static_cast' defined as macro" "" { target c++26 } }
#define struct 1			// { dg-warning "keyword 'struct' defined as macro" "" { target c++26 } }
#define switch 1			// { dg-warning "keyword 'switch' defined as macro" "" { target c++26 } }
#define template 1			// { dg-warning "keyword 'template' defined as macro" "" { target c++26 } }
#define this 1				// { dg-warning "keyword 'this' defined as macro" "" { target c++26 } }
#define thread_local 1			// { dg-warning "keyword 'thread_local' defined as macro" "" { target c++26 } }
#define throw 1				// { dg-warning "keyword 'throw' defined as macro" "" { target c++26 } }
#define true 1				// { dg-warning "keyword 'true' defined as macro" "" { target c++26 } }
#define try 1				// { dg-warning "keyword 'try' defined as macro" "" { target c++26 } }
#define typedef 1			// { dg-warning "keyword 'typedef' defined as macro" "" { target c++26 } }
#define typeid 1			// { dg-warning "keyword 'typeid' defined as macro" "" { target c++26 } }
#define typename 1			// { dg-warning "keyword 'typename' defined as macro" "" { target c++26 } }
#define union 1				// { dg-warning "keyword 'union' defined as macro" "" { target c++26 } }
#define unsigned 1			// { dg-warning "keyword 'unsigned' defined as macro" "" { target c++26 } }
#define using 1				// { dg-warning "keyword 'using' defined as macro" "" { target c++26 } }
#define virtual 1			// { dg-warning "keyword 'virtual' defined as macro" "" { target c++26 } }
#define void 1				// { dg-warning "keyword 'void' defined as macro" "" { target c++26 } }
#define volatile 1			// { dg-warning "keyword 'volatile' defined as macro" "" { target c++26 } }
#define wchar_t 1			// { dg-warning "keyword 'wchar_t' defined as macro" "" { target c++26 } }
#define while 1				// { dg-warning "keyword 'while' defined as macro" "" { target c++26 } }

// [lex.name]
#define final 1				// { dg-warning "keyword 'final' defined as macro" "" { target c++26 } }
#define import 1			// { dg-warning "keyword 'import' defined as macro" "" { target c++26 } }
#define module 1			// { dg-warning "keyword 'module' defined as macro" "" { target c++26 } }
#define override 1			// { dg-warning "keyword 'override' defined as macro" "" { target c++26 } }
#define post 1
#define pre 1
#define replaceable_if_eligible 1	// { dg-warning "keyword 'replaceable_if_eligible' defined as macro" "" { target c++26 } }
#define trivially_relocatable_if_eligible 1	// { dg-warning "keyword 'trivially_relocatable_if_eligible' defined as macro" "" { target c++26 } }

// [dcl.attr]
#define assume 1			// { dg-warning "keyword 'assume' defined as macro" "" { target c++26 } }
#define carries_dependency 1
#define deprecated 1			// { dg-warning "keyword 'deprecated' defined as macro" "" { target c++26 } }
#define fallthrough 1			// { dg-warning "keyword 'fallthrough' defined as macro" "" { target c++26 } }
#define indeterminate 1
#define likely 1			// { dg-warning "keyword 'likely' defined as macro" "" { target c++26 } }
#define maybe_unused 1			// { dg-warning "keyword 'maybe_unused' defined as macro" "" { target c++26 } }
#define nodiscard 1			// { dg-warning "keyword 'nodiscard' defined as macro" "" { target c++26 } }
#define noreturn 1			// { dg-warning "keyword 'noreturn' defined as macro" "" { target c++26 } }
#define no_unique_address 1		// { dg-warning "keyword 'no_unique_address' defined as macro" "" { target c++26 } }
#define unlikely 1			// { dg-warning "keyword 'unlikely' defined as macro" "" { target c++26 } }

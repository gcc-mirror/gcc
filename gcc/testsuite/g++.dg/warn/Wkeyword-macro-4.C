// C++26 P2843R3 - Preprocessing is never undefined
// [cpp.replace.general]/9
// { dg-do preprocess }
// { dg-options "-pedantic-errors" }
// { dg-additional-options "-fmodules" { target c++20 } }

// [lex.key]
#undef alignas 				// { dg-error "undefining keyword 'alignas'" "" { target c++26 } }
#undef alignof 				// { dg-error "undefining keyword 'alignof'" "" { target c++26 } }
#undef asm 				// { dg-error "undefining keyword 'asm'" "" { target c++26 } }
#undef auto 				// { dg-error "undefining keyword 'auto'" "" { target c++26 } }
#undef bool 				// { dg-error "undefining keyword 'bool'" "" { target c++26 } }
#undef break 				// { dg-error "undefining keyword 'break'" "" { target c++26 } }
#undef case 				// { dg-error "undefining keyword 'case'" "" { target c++26 } }
#undef catch 				// { dg-error "undefining keyword 'catch'" "" { target c++26 } }
#undef char 				// { dg-error "undefining keyword 'char'" "" { target c++26 } }
#undef char16_t 			// { dg-error "undefining keyword 'char16_t'" "" { target c++26 } }
#undef char32_t 			// { dg-error "undefining keyword 'char32_t'" "" { target c++26 } }
#undef char8_t 				// { dg-error "undefining keyword 'char8_t'" "" { target c++26 } }
#undef class 				// { dg-error "undefining keyword 'class'" "" { target c++26 } }
#undef co_await 			// { dg-error "undefining keyword 'co_await'" "" { target c++26 } }
#undef concept 				// { dg-error "undefining keyword 'concept'" "" { target c++26 } }
#undef const 				// { dg-error "undefining keyword 'const'" "" { target c++26 } }
#undef const_cast 			// { dg-error "undefining keyword 'const_cast'" "" { target c++26 } }
#undef consteval 			// { dg-error "undefining keyword 'consteval'" "" { target c++26 } }
#undef constexpr 			// { dg-error "undefining keyword 'constexpr'" "" { target c++26 } }
#undef constinit 			// { dg-error "undefining keyword 'constinit'" "" { target c++26 } }
#undef continue 			// { dg-error "undefining keyword 'continue'" "" { target c++26 } }
#undef contract_assert
#undef co_return 			// { dg-error "undefining keyword 'co_return'" "" { target c++26 } }
#undef co_yield 			// { dg-error "undefining keyword 'co_yield'" "" { target c++26 } }
#undef decltype 			// { dg-error "undefining keyword 'decltype'" "" { target c++26 } }
#undef default 				// { dg-error "undefining keyword 'default'" "" { target c++26 } }
#undef delete 				// { dg-error "undefining keyword 'delete'" "" { target c++26 } }
#undef do 				// { dg-error "undefining keyword 'do'" "" { target c++26 } }
#undef double 				// { dg-error "undefining keyword 'double'" "" { target c++26 } }
#undef dynamic_cast 			// { dg-error "undefining keyword 'dynamic_cast'" "" { target c++26 } }
#undef else 				// { dg-error "undefining keyword 'else'" "" { target c++26 } }
#undef enum 				// { dg-error "undefining keyword 'enum'" "" { target c++26 } }
#undef explicit 			// { dg-error "undefining keyword 'explicit'" "" { target c++26 } }
#undef export 				// { dg-error "undefining keyword 'export'" "" { target c++26 } }
#undef extern 				// { dg-error "undefining keyword 'extern'" "" { target c++26 } }
#undef false 				// { dg-error "undefining keyword 'false'" "" { target c++26 } }
#undef float 				// { dg-error "undefining keyword 'float'" "" { target c++26 } }
#undef for 				// { dg-error "undefining keyword 'for'" "" { target c++26 } }
#undef friend 				// { dg-error "undefining keyword 'friend'" "" { target c++26 } }
#undef goto 				// { dg-error "undefining keyword 'goto'" "" { target c++26 } }
#undef if 				// { dg-error "undefining keyword 'if'" "" { target c++26 } }
#undef inline 				// { dg-error "undefining keyword 'inline'" "" { target c++26 } }
#undef int 				// { dg-error "undefining keyword 'int'" "" { target c++26 } }
#undef long 				// { dg-error "undefining keyword 'long'" "" { target c++26 } }
#undef mutable 				// { dg-error "undefining keyword 'mutable'" "" { target c++26 } }
#undef namespace 			// { dg-error "undefining keyword 'namespace'" "" { target c++26 } }
#undef new 				// { dg-error "undefining keyword 'new'" "" { target c++26 } }
#undef noexcept 			// { dg-error "undefining keyword 'noexcept'" "" { target c++26 } }
#undef nullptr 				// { dg-error "undefining keyword 'nullptr'" "" { target c++26 } }
#undef operator 			// { dg-error "undefining keyword 'operator'" "" { target c++26 } }
#undef private 				// { dg-error "undefining keyword 'private'" "" { target c++26 } }
#undef protected 			// { dg-error "undefining keyword 'protected'" "" { target c++26 } }
#undef public 				// { dg-error "undefining keyword 'public'" "" { target c++26 } }
#undef register 			// { dg-error "undefining keyword 'register'" "" { target c++26 } }
#undef reinterpret_cast 		// { dg-error "undefining keyword 'reinterpret_cast'" "" { target c++26 } }
#undef requires 			// { dg-error "undefining keyword 'requires'" "" { target c++26 } }
#undef return 				// { dg-error "undefining keyword 'return'" "" { target c++26 } }
#undef short 				// { dg-error "undefining keyword 'short'" "" { target c++26 } }
#undef signed 				// { dg-error "undefining keyword 'signed'" "" { target c++26 } }
#undef sizeof 				// { dg-error "undefining keyword 'sizeof'" "" { target c++26 } }
#undef static 				// { dg-error "undefining keyword 'static'" "" { target c++26 } }
#undef static_assert 			// { dg-error "undefining keyword 'static_assert'" "" { target c++26 } }
#undef static_cast 			// { dg-error "undefining keyword 'static_cast'" "" { target c++26 } }
#undef struct 				// { dg-error "undefining keyword 'struct'" "" { target c++26 } }
#undef switch 				// { dg-error "undefining keyword 'switch'" "" { target c++26 } }
#undef template 			// { dg-error "undefining keyword 'template'" "" { target c++26 } }
#undef this 				// { dg-error "undefining keyword 'this'" "" { target c++26 } }
#undef thread_local 			// { dg-error "undefining keyword 'thread_local'" "" { target c++26 } }
#undef throw 				// { dg-error "undefining keyword 'throw'" "" { target c++26 } }
#undef true 				// { dg-error "undefining keyword 'true'" "" { target c++26 } }
#undef try 				// { dg-error "undefining keyword 'try'" "" { target c++26 } }
#undef typedef 				// { dg-error "undefining keyword 'typedef'" "" { target c++26 } }
#undef typeid 				// { dg-error "undefining keyword 'typeid'" "" { target c++26 } }
#undef typename 			// { dg-error "undefining keyword 'typename'" "" { target c++26 } }
#undef union 				// { dg-error "undefining keyword 'union'" "" { target c++26 } }
#undef unsigned 			// { dg-error "undefining keyword 'unsigned'" "" { target c++26 } }
#undef using 				// { dg-error "undefining keyword 'using'" "" { target c++26 } }
#undef virtual 				// { dg-error "undefining keyword 'virtual'" "" { target c++26 } }
#undef void 				// { dg-error "undefining keyword 'void'" "" { target c++26 } }
#undef volatile 			// { dg-error "undefining keyword 'volatile'" "" { target c++26 } }
#undef wchar_t 				// { dg-error "undefining keyword 'wchar_t'" "" { target c++26 } }
#undef while 				// { dg-error "undefining keyword 'while'" "" { target c++26 } }

// [lex.name]
#undef final 				// { dg-error "undefining keyword 'final'" "" { target c++26 } }
#undef import 				// { dg-error "undefining keyword 'import'" "" { target c++26 } }
#undef module 				// { dg-error "undefining keyword 'module'" "" { target c++26 } }
#undef override 			// { dg-error "undefining keyword 'override'" "" { target c++26 } }
#undef post
#undef pre
#undef replaceable_if_eligible 	// { dg-error "undefining keyword 'replaceable_if_eligible'" "" { target c++26 } }
#undef trivially_relocatable_if_eligible 	// { dg-error "undefining keyword 'trivially_relocatable_if_eligible'" "" { target c++26 } }

// [dcl.attr]
#undef assume				// { dg-error "undefining keyword 'assume'" "" { target c++26 } }
#undef carries_dependency
#undef deprecated			// { dg-error "undefining keyword 'deprecated'" "" { target c++26 } }
#undef fallthrough			// { dg-error "undefining keyword 'fallthrough'" "" { target c++26 } }
#undef indeterminate
#undef likely				// { dg-error "undefining keyword 'likely'" "" { target c++26 } }
#undef maybe_unused			// { dg-error "undefining keyword 'maybe_unused'" "" { target c++26 } }
#undef nodiscard			// { dg-error "undefining keyword 'nodiscard'" "" { target c++26 } }
#undef noreturn				// { dg-error "undefining keyword 'noreturn'" "" { target c++26 } }
#undef no_unique_address		// { dg-error "undefining keyword 'no_unique_address'" "" { target c++26 } }
#undef unlikely				// { dg-error "undefining keyword 'unlikely'" "" { target c++26 } }

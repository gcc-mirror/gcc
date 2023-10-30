// C++26 P2843R3 - Preprocessing is never undefined
// [cpp.replace.general]/9
// { dg-do preprocess }
// { dg-options "-pedantic" }
// { dg-additional-options "-fmodules" { target c++20 } }

// [lex.key]
#undef alignas 				// { dg-warning "undefining keyword 'alignas'" "" { target c++26 } }
#undef alignof 				// { dg-warning "undefining keyword 'alignof'" "" { target c++26 } }
#undef asm 				// { dg-warning "undefining keyword 'asm'" "" { target c++26 } }
#undef auto 				// { dg-warning "undefining keyword 'auto'" "" { target c++26 } }
#undef bool 				// { dg-warning "undefining keyword 'bool'" "" { target c++26 } }
#undef break 				// { dg-warning "undefining keyword 'break'" "" { target c++26 } }
#undef case 				// { dg-warning "undefining keyword 'case'" "" { target c++26 } }
#undef catch 				// { dg-warning "undefining keyword 'catch'" "" { target c++26 } }
#undef char 				// { dg-warning "undefining keyword 'char'" "" { target c++26 } }
#undef char16_t 			// { dg-warning "undefining keyword 'char16_t'" "" { target c++26 } }
#undef char32_t 			// { dg-warning "undefining keyword 'char32_t'" "" { target c++26 } }
#undef char8_t 				// { dg-warning "undefining keyword 'char8_t'" "" { target c++26 } }
#undef class 				// { dg-warning "undefining keyword 'class'" "" { target c++26 } }
#undef co_await 			// { dg-warning "undefining keyword 'co_await'" "" { target c++26 } }
#undef concept 				// { dg-warning "undefining keyword 'concept'" "" { target c++26 } }
#undef const 				// { dg-warning "undefining keyword 'const'" "" { target c++26 } }
#undef const_cast 			// { dg-warning "undefining keyword 'const_cast'" "" { target c++26 } }
#undef consteval 			// { dg-warning "undefining keyword 'consteval'" "" { target c++26 } }
#undef constexpr 			// { dg-warning "undefining keyword 'constexpr'" "" { target c++26 } }
#undef constinit 			// { dg-warning "undefining keyword 'constinit'" "" { target c++26 } }
#undef continue 			// { dg-warning "undefining keyword 'continue'" "" { target c++26 } }
#undef contract_assert			// { dg-warning "undefining keyword 'contract_assert'" "" { target c++26 } }
#undef co_return 			// { dg-warning "undefining keyword 'co_return'" "" { target c++26 } }
#undef co_yield 			// { dg-warning "undefining keyword 'co_yield'" "" { target c++26 } }
#undef decltype 			// { dg-warning "undefining keyword 'decltype'" "" { target c++26 } }
#undef default 				// { dg-warning "undefining keyword 'default'" "" { target c++26 } }
#undef delete 				// { dg-warning "undefining keyword 'delete'" "" { target c++26 } }
#undef do 				// { dg-warning "undefining keyword 'do'" "" { target c++26 } }
#undef double 				// { dg-warning "undefining keyword 'double'" "" { target c++26 } }
#undef dynamic_cast 			// { dg-warning "undefining keyword 'dynamic_cast'" "" { target c++26 } }
#undef else 				// { dg-warning "undefining keyword 'else'" "" { target c++26 } }
#undef enum 				// { dg-warning "undefining keyword 'enum'" "" { target c++26 } }
#undef explicit 			// { dg-warning "undefining keyword 'explicit'" "" { target c++26 } }
#undef export 				// { dg-warning "undefining keyword 'export'" "" { target c++26 } }
#undef extern 				// { dg-warning "undefining keyword 'extern'" "" { target c++26 } }
#undef false 				// { dg-warning "undefining keyword 'false'" "" { target c++26 } }
#undef float 				// { dg-warning "undefining keyword 'float'" "" { target c++26 } }
#undef for 				// { dg-warning "undefining keyword 'for'" "" { target c++26 } }
#undef friend 				// { dg-warning "undefining keyword 'friend'" "" { target c++26 } }
#undef goto 				// { dg-warning "undefining keyword 'goto'" "" { target c++26 } }
#undef if 				// { dg-warning "undefining keyword 'if'" "" { target c++26 } }
#undef inline 				// { dg-warning "undefining keyword 'inline'" "" { target c++26 } }
#undef int 				// { dg-warning "undefining keyword 'int'" "" { target c++26 } }
#undef long 				// { dg-warning "undefining keyword 'long'" "" { target c++26 } }
#undef mutable 				// { dg-warning "undefining keyword 'mutable'" "" { target c++26 } }
#undef namespace 			// { dg-warning "undefining keyword 'namespace'" "" { target c++26 } }
#undef new 				// { dg-warning "undefining keyword 'new'" "" { target c++26 } }
#undef noexcept 			// { dg-warning "undefining keyword 'noexcept'" "" { target c++26 } }
#undef nullptr 				// { dg-warning "undefining keyword 'nullptr'" "" { target c++26 } }
#undef operator 			// { dg-warning "undefining keyword 'operator'" "" { target c++26 } }
#undef private 				// { dg-warning "undefining keyword 'private'" "" { target c++26 } }
#undef protected 			// { dg-warning "undefining keyword 'protected'" "" { target c++26 } }
#undef public 				// { dg-warning "undefining keyword 'public'" "" { target c++26 } }
#undef register 			// { dg-warning "undefining keyword 'register'" "" { target c++26 } }
#undef reinterpret_cast 		// { dg-warning "undefining keyword 'reinterpret_cast'" "" { target c++26 } }
#undef requires 			// { dg-warning "undefining keyword 'requires'" "" { target c++26 } }
#undef return 				// { dg-warning "undefining keyword 'return'" "" { target c++26 } }
#undef short 				// { dg-warning "undefining keyword 'short'" "" { target c++26 } }
#undef signed 				// { dg-warning "undefining keyword 'signed'" "" { target c++26 } }
#undef sizeof 				// { dg-warning "undefining keyword 'sizeof'" "" { target c++26 } }
#undef static 				// { dg-warning "undefining keyword 'static'" "" { target c++26 } }
#undef static_assert 			// { dg-warning "undefining keyword 'static_assert'" "" { target c++26 } }
#undef static_cast 			// { dg-warning "undefining keyword 'static_cast'" "" { target c++26 } }
#undef struct 				// { dg-warning "undefining keyword 'struct'" "" { target c++26 } }
#undef switch 				// { dg-warning "undefining keyword 'switch'" "" { target c++26 } }
#undef template 			// { dg-warning "undefining keyword 'template'" "" { target c++26 } }
#undef this 				// { dg-warning "undefining keyword 'this'" "" { target c++26 } }
#undef thread_local 			// { dg-warning "undefining keyword 'thread_local'" "" { target c++26 } }
#undef throw 				// { dg-warning "undefining keyword 'throw'" "" { target c++26 } }
#undef true 				// { dg-warning "undefining keyword 'true'" "" { target c++26 } }
#undef try 				// { dg-warning "undefining keyword 'try'" "" { target c++26 } }
#undef typedef 				// { dg-warning "undefining keyword 'typedef'" "" { target c++26 } }
#undef typeid 				// { dg-warning "undefining keyword 'typeid'" "" { target c++26 } }
#undef typename 			// { dg-warning "undefining keyword 'typename'" "" { target c++26 } }
#undef union 				// { dg-warning "undefining keyword 'union'" "" { target c++26 } }
#undef unsigned 			// { dg-warning "undefining keyword 'unsigned'" "" { target c++26 } }
#undef using 				// { dg-warning "undefining keyword 'using'" "" { target c++26 } }
#undef virtual 				// { dg-warning "undefining keyword 'virtual'" "" { target c++26 } }
#undef void 				// { dg-warning "undefining keyword 'void'" "" { target c++26 } }
#undef volatile 			// { dg-warning "undefining keyword 'volatile'" "" { target c++26 } }
#undef wchar_t 				// { dg-warning "undefining keyword 'wchar_t'" "" { target c++26 } }
#undef while 				// { dg-warning "undefining keyword 'while'" "" { target c++26 } }

// [lex.name]
#undef final 				// { dg-warning "undefining keyword 'final'" "" { target c++26 } }
#undef import 				// { dg-warning "undefining keyword 'import'" "" { target c++26 } }
#undef module 				// { dg-warning "undefining keyword 'module'" "" { target c++26 } }
#undef override 			// { dg-warning "undefining keyword 'override'" "" { target c++26 } }
#undef post
#undef pre
#undef replaceable_if_eligible
#undef trivially_relocatable_if_eligible

// [dcl.attr]
#undef assume				// { dg-warning "undefining keyword 'assume'" "" { target c++26 } }
#undef carries_dependency
#undef deprecated			// { dg-warning "undefining keyword 'deprecated'" "" { target c++26 } }
#undef fallthrough			// { dg-warning "undefining keyword 'fallthrough'" "" { target c++26 } }
#undef indeterminate			// { dg-warning "undefining keyword 'indeterminate'" "" { target c++26 } }
#undef likely
#undef maybe_unused			// { dg-warning "undefining keyword 'maybe_unused'" "" { target c++26 } }
#undef nodiscard			// { dg-warning "undefining keyword 'nodiscard'" "" { target c++26 } }
#undef noreturn				// { dg-warning "undefining keyword 'noreturn'" "" { target c++26 } }
#undef no_unique_address		// { dg-warning "undefining keyword 'no_unique_address'" "" { target c++26 } }
#undef unlikely

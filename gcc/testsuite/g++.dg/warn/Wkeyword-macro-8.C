// C++26 P2843R3 - Preprocessing is never undefined
// [cpp.replace.general]/9
// { dg-do preprocess }
// { dg-options "-Wkeyword-macro" }
// { dg-additional-options "-fmodules" { target c++20 } }
// { dg-additional-options "-fcontracts" { target c++26 } }

// [lex.key]
#undef alignas				// { dg-warning "undefining keyword 'alignas'" "" { target c++11 } }
#undef alignof				// { dg-warning "undefining keyword 'alignof'" "" { target c++11 } }
#undef asm				// { dg-warning "undefining keyword 'asm'" }
#undef auto				// { dg-warning "undefining keyword 'auto'" }
#undef bool				// { dg-warning "undefining keyword 'bool'" }
#undef break				// { dg-warning "undefining keyword 'break'" }
#undef case				// { dg-warning "undefining keyword 'case'" }
#undef catch				// { dg-warning "undefining keyword 'catch'" }
#undef char				// { dg-warning "undefining keyword 'char'" }
#undef char16_t				// { dg-warning "undefining keyword 'char16_t'" "" { target c++11 } }
#undef char32_t				// { dg-warning "undefining keyword 'char32_t'" "" { target c++11 } }
#undef char8_t				// { dg-warning "undefining keyword 'char8_t'" "" { target c++20 } }
#undef class				// { dg-warning "undefining keyword 'class'" }
#undef co_await				// { dg-warning "undefining keyword 'co_await'" "" { target c++20 } }
#undef concept				// { dg-warning "undefining keyword 'concept'" "" { target c++20 } }
#undef const				// { dg-warning "undefining keyword 'const'" }
#undef const_cast			// { dg-warning "undefining keyword 'const_cast'" }
#undef consteval			// { dg-warning "undefining keyword 'consteval'" "" { target c++20 } }
#undef constexpr			// { dg-warning "undefining keyword 'constexpr'" "" { target c++11 } }
#undef constinit			// { dg-warning "undefining keyword 'constinit'" "" { target c++20 } }
#undef continue				// { dg-warning "undefining keyword 'continue'" }
#undef contract_assert			// { dg-warning "undefining keyword 'contract_assert'" "" { target c++26 } }
#undef co_return			// { dg-warning "undefining keyword 'co_return'" "" { target c++20 } }
#undef co_yield				// { dg-warning "undefining keyword 'co_yield'" "" { target c++20 } }
#undef decltype				// { dg-warning "undefining keyword 'decltype'" "" { target c++11 } }
#undef default				// { dg-warning "undefining keyword 'default'" }
#undef delete				// { dg-warning "undefining keyword 'delete'" }
#undef do				// { dg-warning "undefining keyword 'do'" }
#undef double				// { dg-warning "undefining keyword 'double'" }
#undef dynamic_cast			// { dg-warning "undefining keyword 'dynamic_cast'" }
#undef else				// { dg-warning "undefining keyword 'else'" }
#undef enum				// { dg-warning "undefining keyword 'enum'" }
#undef explicit				// { dg-warning "undefining keyword 'explicit'" }
#undef export				// { dg-warning "undefining keyword 'export'" }
#undef extern				// { dg-warning "undefining keyword 'extern'" }
#undef false				// { dg-warning "undefining keyword 'false'" }
#undef float				// { dg-warning "undefining keyword 'float'" }
#undef for				// { dg-warning "undefining keyword 'for'" }
#undef friend				// { dg-warning "undefining keyword 'friend'" }
#undef goto				// { dg-warning "undefining keyword 'goto'" }
#undef if				// { dg-warning "undefining keyword 'if'" }
#undef inline				// { dg-warning "undefining keyword 'inline'" }
#undef int				// { dg-warning "undefining keyword 'int'" }
#undef long				// { dg-warning "undefining keyword 'long'" }
#undef mutable				// { dg-warning "undefining keyword 'mutable'" }
#undef namespace			// { dg-warning "undefining keyword 'namespace'" }
#undef new				// { dg-warning "undefining keyword 'new'" }
#undef noexcept				// { dg-warning "undefining keyword 'noexcept'" "" { target c++11 } }
#undef nullptr				// { dg-warning "undefining keyword 'nullptr'" "" { target c++11 } }
#undef operator				// { dg-warning "undefining keyword 'operator'" }
#undef private				// { dg-warning "undefining keyword 'private'" }
#undef protected			// { dg-warning "undefining keyword 'protected'" }
#undef public				// { dg-warning "undefining keyword 'public'" }
#undef register				// { dg-warning "undefining keyword 'register'" }
#undef reinterpret_cast			// { dg-warning "undefining keyword 'reinterpret_cast'" }
#undef requires				// { dg-warning "undefining keyword 'requires'" "" { target c++20 } }
#undef return				// { dg-warning "undefining keyword 'return'" }
#undef short				// { dg-warning "undefining keyword 'short'" }
#undef signed				// { dg-warning "undefining keyword 'signed'" }
#undef sizeof				// { dg-warning "undefining keyword 'sizeof'" }
#undef static				// { dg-warning "undefining keyword 'static'" }
#undef static_assert			// { dg-warning "undefining keyword 'static_assert'" "" { target c++11 } }
#undef static_cast			// { dg-warning "undefining keyword 'static_cast'" }
#undef struct				// { dg-warning "undefining keyword 'struct'" }
#undef switch				// { dg-warning "undefining keyword 'switch'" }
#undef template				// { dg-warning "undefining keyword 'template'" }
#undef this				// { dg-warning "undefining keyword 'this'" }
#undef thread_local			// { dg-warning "undefining keyword 'thread_local'" "" { target c++11 } }
#undef throw				// { dg-warning "undefining keyword 'throw'" }
#undef true				// { dg-warning "undefining keyword 'true'" }
#undef try				// { dg-warning "undefining keyword 'try'" }
#undef typedef				// { dg-warning "undefining keyword 'typedef'" }
#undef typeid				// { dg-warning "undefining keyword 'typeid'" }
#undef typename				// { dg-warning "undefining keyword 'typename'" }
#undef union				// { dg-warning "undefining keyword 'union'" }
#undef unsigned				// { dg-warning "undefining keyword 'unsigned'" }
#undef using				// { dg-warning "undefining keyword 'using'" }
#undef virtual				// { dg-warning "undefining keyword 'virtual'" }
#undef void				// { dg-warning "undefining keyword 'void'" }
#undef volatile				// { dg-warning "undefining keyword 'volatile'" }
#undef wchar_t				// { dg-warning "undefining keyword 'wchar_t'" }
#undef while				// { dg-warning "undefining keyword 'while'" }

// [lex.name]
#undef final				// { dg-warning "undefining keyword 'final'" "" { target c++11 } }
#undef import				// { dg-warning "undefining keyword 'import'" "" { target c++20 } }
#undef module				// { dg-warning "undefining keyword 'module'" "" { target c++20 } }
#undef override				// { dg-warning "undefining keyword 'override'" "" { target c++11 } }
#undef post
#undef pre
#undef replaceable_if_eligible
#undef trivially_relocatable_if_eligible

// [dcl.attr]
#undef assume				// { dg-warning "undefining keyword 'assume'" "" { target c++23 } }
#undef carries_dependency		// { dg-warning "undefining keyword 'carries_dependency'" "" { target { c++11 && c++23_down } } }
#undef deprecated			// { dg-warning "undefining keyword 'deprecated'" "" { target c++14 } }
#undef fallthrough			// { dg-warning "undefining keyword 'fallthrough'" "" { target c++17 } }
#undef indeterminate			// { dg-warning "undefining keyword 'indeterminate'" "" { target c++26 } }
#undef likely
#undef maybe_unused			// { dg-warning "undefining keyword 'maybe_unused'" "" { target c++17 } }
#undef nodiscard			// { dg-warning "undefining keyword 'nodiscard'" "" { target c++17 } }
#undef noreturn				// { dg-warning "undefining keyword 'noreturn'" "" { target c++11 } }
#undef no_unique_address		// { dg-warning "undefining keyword 'no_unique_address'" "" { target c++20 } }
#undef unlikely

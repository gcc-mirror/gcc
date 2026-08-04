// PR c++/125820
// P4101R1, Consteval-only Values for C++26
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

using info = decltype(^^int);

void
uneval ()
{
  auto a = sizeof(^^int);
  auto b = alignof (decltype (^^int));
  auto c = noexcept (^^int == ^^void);
  auto d = sizeof (info);
  auto e = requires { ^^int; };
  __extension__ using U = decltype (({ ^^int; }));
}

void
discarded ()
{
  (void) ^^int;					// { dg-error "consteval-only" }
  static_cast<void> (^^int);			// { dg-error "consteval-only" }
  ^^int;					// { dg-error "consteval-only" }
  ^^int, 42;					// { dg-error "consteval-only" }
  auto b1 = (^^int, 42);			// { dg-error "consteval-only" }
  auto b2 = (42, ^^int);			// { dg-error "consteval-only" }
  __extension__ auto b3 = ({ (^^int, 42); });	// { dg-error "consteval-only" }
  __extension__ auto b4 = ({ (42, ^^int); });	// { dg-error "consteval-only" }
  auto b5 = (^^int == ^^void, 42);		// { dg-error "consteval-only" }
}

void
stmt_exprs (bool cond)
{
  __extension__ auto c1 = ({ ^^int; 42; });			// { dg-error "consteval-only" }
  __extension__ auto c2 = ({ 42; ^^int; });			// { dg-error "consteval-only" }
  __extension__ auto c3 = ({ ({ ^^int; }); });			// { dg-error "consteval-only" }
  __extension__ auto c4 = ({ ({ ^^int; }); 42; });		// { dg-error "consteval-only" }
  __extension__ auto c5 = ({ if (cond) ; ^^int; });		// { dg-error "consteval-only" }
  __extension__ auto c6 = ({ if (cond) (void) ^^int; 42; });	// { dg-error "consteval-only" }
  __extension__ auto c7 = ({ while (0) { ^^int; } 42; });	// { dg-error "consteval-only" }
  __extension__ auto c8 = ({ for (int i = 0; i < 2; ++i) { ^^int; } 42; });	// { dg-error "consteval-only" }
  __extension__ auto c9 = ({ switch (cond) { case 0: ^^int; } 42; });		// { dg-error "consteval-only" }
  __extension__ auto c10 = ({ info x = ^^int; 42; });		// { dg-error "consteval-only" }
  __extension__ auto c11 = ({ constexpr info x = ^^int; 42; });
  __extension__ auto c12 = ({ constexpr info x = ^^int; x == ^^void; });	// { dg-error "consteval-only" }
  __extension__ auto c13 = ({ constexpr info x = ^^int; x; });	// { dg-error "consteval-only" }
  __extension__ auto c14 = ({ cond ? ^^int : ^^void; });	// { dg-error "consteval-only" }
  __extension__ auto c15 = ({ true ? ^^int : ^^void; });	// { dg-error "consteval-only" }
  __extension__ auto c16 = ({ int i = 0; i; });
  __extension__ auto c17 = ({ ^^int; });			// { dg-error "consteval-only" }
  __extension__ constexpr auto c18 = ({ ^^int; });
  __extension__ constexpr bool c19 = ({ ^^int == ^^void; });
  __extension__ auto c20 = ({ [] { return ^^int == ^^void; } (); });
  __extension__ auto c21 = ({ struct S { info r; }; S{}.r; });
  __extension__ auto c22 = ({ using U = info; U{}; });
  __extension__ auto c23 = ({ using U = decltype(^^int); U{}; });
  __extension__ auto c24 = ({ using U = decltype(^^int); U u = ^^int; u; });	// { dg-error "consteval-only" }
  __extension__ auto c25 = ({ if constexpr (false) ^^int; 42; });
  __extension__ auto c26 = ({ if constexpr (true) ^^int; 42; });		// { dg-error "consteval-only" }
  __extension__ auto c27 = ({ if consteval { 42; } else { ^^int; } 42; });	// { dg-error "consteval-only" }
  __extension__ auto c28 = ({ if ! consteval { 42; } else { ^^int; } 42; });
}

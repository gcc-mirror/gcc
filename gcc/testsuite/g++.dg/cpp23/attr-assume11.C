// PR c++/109756
// { dg-do compile { target c++11 } }

template <int ...args>
void
foo ()
{
  [[assume (1 > 0)...]];		// { dg-error "expansion pattern '\\\(1 > 0\\\)' contains no parameter packs" }
					// { dg-warning "attributes at the beginning of statement are ignored" "" { target *-*-* } .-1 }
  [[assume (args > 0)...]];		// { dg-error "pack expansion of 'assume' attribute" }
					// { dg-message "use fold expression in the attribute argument instead" "" { target c++17 } .-1 }
#if __cpp_fold_expressions >= 201603L
  [[assume (((args > 0) && ...))]];
#endif
  [[gnu::assume (1 > 0)...]];		// { dg-error "expansion pattern '\\\(1 > 0\\\)' contains no parameter packs" }
					// { dg-warning "attributes at the beginning of statement are ignored" "" { target *-*-* } .-1 }
  [[gnu::assume (args > 0)...]];	// { dg-error "pack expansion of 'assume' attribute" }
					// { dg-message "use fold expression in the attribute argument instead" "" { target c++17 } .-1  }
#if __cpp_fold_expressions >= 201603L
  [[gnu::assume (((args > 0) && ...))]];
#endif
}

// PR c++/109756
// { dg-do compile { target c++11 } }
// { dg-options "-Wno-attributes" }

template <int ...args>
[[noreturn...]]					// { dg-error "attribute with no arguments contains no parameter packs" }
[[deprecated...]]				// { dg-error "attribute with no arguments contains no parameter packs" }
[[nodiscard...]]				// { dg-error "attribute with no arguments contains no parameter packs" }
int foo (int x)
{
  switch (x)
    {
    case 1:
      [[likely...]];				// { dg-error "attribute with no arguments contains no parameter packs" }
      [[fallthrough...]];			// { dg-error "attribute with no arguments contains no parameter packs" }
    case 2:
      [[unlikely...]];				// { dg-error "attribute with no arguments contains no parameter packs" }

      break;
    default:
      break;
    }
  struct T {};
  struct S { [[no_unique_address...]] T t; };	// { dg-error "attribute with no arguments contains no parameter packs" }
  for (;;)
    ;
}

int a = foo <1, 2, 3> (4);

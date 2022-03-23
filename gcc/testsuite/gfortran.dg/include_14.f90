! { dg-additional-options "-cpp -idirafter /fdaf/ -I bar -J foo/bar" }
end
! default: warn for -I and -J but ignore other options.
! { dg-warning "Nonexistent include directory 'bar'" "" { target *-*-* } 0 }
! { dg-warning "Nonexistent include directory 'foo/bar'" "" { target *-*-* } 0 }


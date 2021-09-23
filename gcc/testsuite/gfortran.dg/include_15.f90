! { dg-additional-options "-cpp -idirafter /fdaf/ -I bar -J foo/bar -Wmissing-include-dirs" }
end

! { dg-warning " /fdaf/: No such file or directory" "" { target *-*-* } 0 }
! { dg-warning " bar: No such file or directory" "" { target *-*-* } 0 }
! { dg-warning " foo/bar: No such file or directory" "" { target *-*-* } 0 }

! { dg-additional-options "-cpp -idirafter /fdaf/ -I bar -J foo/bar -Wmissing-include-dirs" }
end

! { dg-warning " /fdaf/: No such file or directory" "" { target *-*-* } 0 }
! { dg-warning " bar: No such file or directory" "" { target *-*-* } 0 }
! { dg-warning " foo/bar: No such file or directory" "" { target *-*-* } 0 }

! Depending how the testsuite is run, it may or may not print the following warning:
! { dg-prune-output "Warning: finclude: No such file or directory" }

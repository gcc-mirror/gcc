! { dg-do compile }
! { dg-options "-fdiagnostics-show-caret" }
! { dg-allow-blank-lines-in-output 1 }

program main
  goto 1000
1000 continue ! first instance
  a = a
  a = a
  a = a
1000 continue ! second instance
end

#if 0
! { dg-locus "4" "" { target *-*-* } "7" }
! { dg-begin-multiline-output "" }

 1000 continue ! first instance
    1
! { dg-end-multiline-output "" }
! { dg-locus "4" "" { target *-*-* } "11" }
! { dg-begin-multiline-output "" }

 1000 continue ! second instance
    2
Error: Duplicate statement label 1000 at (1) and (2)
! { dg-end-multiline-output "" }
#endif

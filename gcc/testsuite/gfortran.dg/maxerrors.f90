! { dg-do compile }
! { dg-options "-fmax-errors=1" }
! PR66528
! { dg-prune-output "compilation terminated" }
program main
  read (*,*) n
  if (n<0) then
    print *,foo
  end ! { dg-error "END IF statement expected" }
    print *,bar
end program main


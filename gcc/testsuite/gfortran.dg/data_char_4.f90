! { dg-do compile }
! PR fortran/99205 - Out of memory with undefined character length 
! { dg-options "-w" }

program p
  character(l) :: c(2) ! { dg-error "must have constant character length" }
  data c /'a', 'b'/
  common c
end

! { dg-error "cannot appear in the expression at" " " { target *-*-* } 6 }

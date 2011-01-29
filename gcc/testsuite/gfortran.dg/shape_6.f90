! { dg-do compile }
!
! PR fortran/47531
!
! Contributed by James Van Buskirk
!
! Check for the presence of the optional kind= argument
! of F2003.
!

program bug1
   use ISO_C_BINDING
   implicit none
   real,allocatable :: weevil(:,:)

   write(*,*) achar(64,C_CHAR)
   write(*,*) char(64,C_CHAR)
   write(*,*) iachar('A',C_INTPTR_T)
   write(*,*) ichar('A',C_INTPTR_T)
   write(*,*) len('A',C_INTPTR_T)
   write(*,*) len_trim('A',C_INTPTR_T)
   allocate(weevil(2,2))
   weevil = 42
   write(*,*) ceiling(weevil,C_INTPTR_T)
   write(*,*) floor(weevil,C_INTPTR_T)
   write(*,*) shape(weevil,C_INTPTR_T)
   write(*,*) storage_size(weevil,C_INTPTR_T)
end program bug1


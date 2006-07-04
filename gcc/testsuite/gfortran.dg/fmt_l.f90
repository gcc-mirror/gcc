! { dg-do run }
! { dg-options "-std=gnu -pedantic -ffree-line-length-none" }
! Test the GNU extension of a L format descriptor without width
! PR libfortran/21303
program test_l
  logical(kind=1) :: l1
  logical(kind=2) :: l2
  logical(kind=4) :: l4
  logical(kind=8) :: l8

  character(len=20) :: str

  l1 = .true.
  write (str,"(L)") l1 ! { dg-warning "Extension: Missing positive width after L descriptor" }
  read (str,"(L)") l1 ! { dg-warning "Extension: Missing positive width after L descriptor" }
  if (l1 .neqv. .true.) call abort

  l2 = .true.
  write (str,"(L)") l2 ! { dg-warning "Extension: Missing positive width after L descriptor" }
  read (str,"(L)") l2 ! { dg-warning "Extension: Missing positive width after L descriptor" }
  if (l2 .neqv. .true.) call abort

  l4 = .true.
  write (str,"(L)") l4 ! { dg-warning "Extension: Missing positive width after L descriptor" }
  read (str,"(L)") l4 ! { dg-warning "Extension: Missing positive width after L descriptor" }
  if (l4 .neqv. .true.) call abort

  l8 = .true.
  write (str,"(L)") l8 ! { dg-warning "Extension: Missing positive width after L descriptor" }
  read (str,"(L)") l8 ! { dg-warning "Extension: Missing positive width after L descriptor" }
  if (l8 .neqv. .true.) call abort

  l1 = .false.
  write (str,"(L)") l1 ! { dg-warning "Extension: Missing positive width after L descriptor" }
  read (str,"(L)") l1 ! { dg-warning "Extension: Missing positive width after L descriptor" }
  if (l1 .neqv. .false.) call abort

  l2 = .false.
  write (str,"(L)") l2 ! { dg-warning "Extension: Missing positive width after L descriptor" }
  read (str,"(L)") l2 ! { dg-warning "Extension: Missing positive width after L descriptor" }
  if (l2 .neqv. .false.) call abort

  l4 = .false.
  write (str,"(L)") l4 ! { dg-warning "Extension: Missing positive width after L descriptor" }
  read (str,"(L)") l4 ! { dg-warning "Extension: Missing positive width after L descriptor" }
  if (l4 .neqv. .false.) call abort

  l8 = .false.
  write (str,"(L)") l8 ! { dg-warning "Extension: Missing positive width after L descriptor" }
  read (str,"(L)") l8 ! { dg-warning "Extension: Missing positive width after L descriptor" }
  if (l8 .neqv. .false.) call abort

end program test_l
! { dg-output "At line 14 of file.*" }
! { dg-output "Fortran runtime warning: Positive width required in format\n" }
! { dg-output "At line 15 of file.*" }
! { dg-output "Fortran runtime warning: Positive width required in format\n" }
! { dg-output "At line 19 of file.*" }
! { dg-output "Fortran runtime warning: Positive width required in format\n" }
! { dg-output "At line 20 of file.*" }
! { dg-output "Fortran runtime warning: Positive width required in format\n" }
! { dg-output "At line 24 of file.*" }
! { dg-output "Fortran runtime warning: Positive width required in format\n" }
! { dg-output "At line 25 of file.*" }
! { dg-output "Fortran runtime warning: Positive width required in format\n" }
! { dg-output "At line 29 of file.*" }
! { dg-output "Fortran runtime warning: Positive width required in format\n" }
! { dg-output "At line 30 of file.*" }
! { dg-output "Fortran runtime warning: Positive width required in format\n" }
! { dg-output "At line 34 of file.*" }
! { dg-output "Fortran runtime warning: Positive width required in format\n" }
! { dg-output "At line 35 of file.*" }
! { dg-output "Fortran runtime warning: Positive width required in format\n" }
! { dg-output "At line 39 of file.*" }
! { dg-output "Fortran runtime warning: Positive width required in format\n" }
! { dg-output "At line 40 of file.*" }
! { dg-output "Fortran runtime warning: Positive width required in format\n" }
! { dg-output "At line 44 of file.*" }
! { dg-output "Fortran runtime warning: Positive width required in format\n" }
! { dg-output "At line 45 of file.*" }
! { dg-output "Fortran runtime warning: Positive width required in format\n" }
! { dg-output "At line 49 of file.*" }
! { dg-output "Fortran runtime warning: Positive width required in format\n" }
! { dg-output "At line 50 of file.*" }
! { dg-output "Fortran runtime warning: Positive width required in format\n" }

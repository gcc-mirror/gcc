! { dg-do compile }
! { dg-options "-std=f95" }
! Part I of the test  of the IO constraints patch, which fixes PRs:
! PRs 25053, 25063, 25064, 25066, 25067, 25068, 25069, 25307 and 20862.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
module fails

 2000 format (1h , 2i6)                        ! { dg-error "Format statement in module" }

end module fails

module global

  integer :: modvar
  namelist /NL/ modvar

contains

  subroutine foo (i)
    integer :: i
    write (*, 100) i
 100 format (1h , "i=", i6)                     ! { dg-warning "The H format specifier at ... is a Fortran 95 deleted feature" }
  end subroutine foo

end module global

 use global
 integer :: a,b, c(20)
 integer(8) :: ierr
 character(80) :: buffer(3)

! Appending to a USE associated namelist is an extension.

 NAMELIST /NL/ a,b                              ! { dg-error "already is USE associated" }

 a=1 ; b=2

!9.2.2.1:
 write(c, *) a, b                               !  { dg-error "array" }
!Was correctly picked up before patch.
 write(buffer((/3,1,2/)), *) a, b               !  { dg-error "vector subscript" }

!9.2.2.2 and one of 9.4.1
!________________________

 write(6, NML=NL, FMT = '(i6)')                 !  { dg-error "group name and format" }
 write(6, NML=NL, FMT = 200)                    !  { dg-error "group name and format" }

!9.4.1
!_____
!

! R912
!Was correctly picked up before patch.
 write(6, NML=NL, iostat = ierr)                ! { dg-error "requires default INTEGER" }

! Constraints
!Was correctly picked up before patch.
 write(1, fmt='(i6)', end = 100) a              ! { dg-error "END tag" }
!Was correctly picked up before patch.
 write(1, fmt='(i6)', eor = 100) a              ! { dg-error "EOR tag" }
!Was correctly picked up before patch.
 write(1, fmt='(i6)', size = b) a               ! { dg-error "SIZE= specifier not allowed" }


 READ(1, fmt='(i6)', end = 900) a               ! { dg-error "not defined" }
 READ(1, fmt='(i6)', eor = 900, advance='NO') a ! { dg-error "not defined" }
 READ(1, fmt='(i6)', ERR = 900) a               ! { dg-error "not defined" }

!Was correctly picked up before patch.
 READ(1, fmt=800) a                             ! { dg-error "not defined" }


100 continue
200 format (2i6)
 END

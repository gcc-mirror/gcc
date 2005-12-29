! { dg-do compile }
! Part II of the test  of the IO constraints patch, which fixes PRs:
! PRs 25053, 25063, 25064, 25066, 25067, 25068, 25069, 25307 and 20862.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!

module global

  integer :: modvar
  namelist /NL/ modvar

contains

  subroutine foo (i)
    integer :: i
    write (*, 100) i
 100 format (1h , "i=", i6)                     ! This is OK.
  end subroutine foo

end module global

 use global
 integer :: a,b, c(20)
 integer(8) :: ierr
 character*80 :: buffer(3)


! Appending to a USE associated namelist is an extension.

 NAMELIST /NL/ a,b                              ! { dg-warning "already is USE associated" }

 a=1 ; b=2

 write(*, NML=NL) z                             !  { dg-error "followed by IO-list" }
!Was correctly picked up before patch.
 print NL, z                                    !  { dg-error "followed by IO-list" }
!
! Not allowed with internal unit
!Was correctly picked up before patch.
 write(buffer, NML=NL)                          !  { dg-error "incompatible with namelist" }
!Was correctly picked up before patch.
 write(buffer, fmt='(i6)', REC=10) a            !  { dg-error "REC tag" }
 write(buffer, fmt='(i6)', END=10) a            !  { dg-error "END tag" }

! Not allowed with REC= specifier
!Was correctly picked up before patch.
 read(10, REC=10, END=100)                      !  { dg-error "END tag is not allowed" }
 write(*, *, REC=10)                            !  { dg-error "FMT=" }

! Not allowed with an ADVANCE=specifier
 READ(buffer, fmt='(i6)', advance='YES') a      ! { dg-error "internal file" }
 READ(1, NML=NL, advance='YES')                 ! { dg-error "NAMELIST IO is not allowed" }

 write(1, fmt='(i6)', advance='YES', size = c(1)) a ! { dg-error "output" }
 write(1, fmt='(i6)', advance='YES', eor = 100) a   ! { dg-error "output" }

 read(1, fmt='(i6)', advance='YES', size = c(1)) a  ! { dg-error "ADVANCE = 'NO'" }
 read(1, fmt='(i6)', advance='YES', eor = 100) a    ! { dg-error "ADVANCE = 'NO'" }

 READ(1, fmt='(i6)', advance='NO', size = buffer) a ! { dg-error "INTEGER" }
!Was correctly picked up before patch. -correct syntax error
 READ(1, fmt='(i6)', advance='YES', size = 10) a    ! { dg-error "Syntax error" }

 READ(1, fmt='(i6)', advance='MAYBE')               !  { dg-error "YES or NO" }

100 continue
200 format (2i6)
 END

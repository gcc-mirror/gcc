! { dg-do compile }
! Tests fix for PR20840 - would ICE with vector subscript in 
! internal unit.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
  character(len=12), dimension(4) :: iu, buff
  character(len=48), dimension(2) :: iue
  equivalence (iu, iue)
  integer, dimension(4) :: v = (/2,1,4,3/)
  iu = (/"Vector    ","subscripts","not       ","allowed!  "/)
  read (iu, '(a12/)') buff
  read (iue(1), '(4a12)') buff
  read (iu(4:1:-1), '(a12/)') buff
  read (iu(v), '(a12/)') buff           ! { dg-error "with vector subscript" }
  read (iu((/2,4,3,1/)), '(a12/)') buff ! { dg-error "with vector subscript" }
  print *, buff
  end


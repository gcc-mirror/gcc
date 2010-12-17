!{ dg-do compile }
! this routine tests all the execution paths
! through the routine known as match_old_style_init()
! it does not make sense in any other context !!
      subroutine sub1(Z) !{ dg-error "DATA attribute conflicts" }
      integer Z/10/!{ dg-error "DATA"}
      end
      pure function pi(k)
      integer ,intent(in) :: k
      integer i / 10 / !{ dg-error "Initialization at " }
      pi=3.0
      end function pi
      subroutine sub2
      integer I /  /!{ dg-error "Syntax error in DATA" }
      end

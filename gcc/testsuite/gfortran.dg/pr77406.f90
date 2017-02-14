! { dg-do compile }
! { dg-options "-w" }
module m
   interface s
      subroutine s1(*)  ! { dg-error "Ambiguous interfaces" }
      end
      subroutine s2(*)  ! { dg-error "Ambiguous interfaces" }
      end
   end interface 
   interface t
      subroutine t1(*)
      end
      subroutine t2(*,*)
      end
   end interface
   interface u
      subroutine u1(*,x)
      end
      subroutine u2(*,i)
      end
   end interface
   interface v
      subroutine v1(*,*)  ! { dg-error "Ambiguous interfaces" }
      end
      subroutine v2(*,*)  ! { dg-error "Ambiguous interfaces" }
      end
   end interface
   interface w
      subroutine w1(*,i)  ! { dg-error "Ambiguous interfaces" }
      end
      subroutine w2(*,j)  ! { dg-error "Ambiguous interfaces" }
      end
   end interface
end

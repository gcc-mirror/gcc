! { dg-do compile }
!
! PR fortran/50408
!
! Contributed by Vittorio Zecca
!
       module m
         type int
           integer  :: val
         end type int
         interface ichar
           module procedure uch
        end interface
       contains
         function uch (c)
           character (len=1), intent (in) :: c
           type (int)                     :: uch
           intrinsic ichar
           uch%val = 127 - ichar (c)
         end function uch 
       end module m

      program p
        use m
        print *,ichar('~') ! must print "1"
      end program p

! { dg-final { cleanup-modules "m" } }

! { dg-do compile }
! PR 62242
! Array constructor with an array element whose value is a
! character function that is described in an interface block and which
! has an assumed-length result
module gfbug
    implicit none
    INTERFACE
      function UpperCase(string) result(upper) 
          character(*), intent(IN) :: string
          character(LEN(string)) :: upper
      end function
      function f2(string) result(upper) 
          character(*), intent(IN) :: string
          character(5) :: upper
      end function
    END INTERFACE
contains
    subroutine s1
        character(5) c
        character(5), dimension(1) :: ca
        ca = (/f2(c)/)  ! This compiles
        ca = (/Uppercase(c)/) ! This gets an ICE
    end subroutine
end module gfbug


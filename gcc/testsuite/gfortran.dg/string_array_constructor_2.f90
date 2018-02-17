! { dg-do run }
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
        character(5), dimension(1) :: cb
        c = "12345"
        ca = (/f2(c)/) ! This works
        !print *, ca(1)
        cb = (/Uppercase(c)/) ! This gets an ICE
        if (ca(1) .ne. cb(1)) then
            STOP 1
        end if
        !print *, ca(1)
    end subroutine
end module gfbug

function UpperCase(string) result(upper) 
    character(*), intent(IN) :: string
    character(LEN(string)) :: upper
    upper = string
end function
function f2(string) result(upper) 
    character(*), intent(IN) :: string
    character(5) :: upper
    upper = string
end function

program main
    use gfbug
    call s1
end program

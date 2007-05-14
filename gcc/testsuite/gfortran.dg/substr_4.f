! { dg-do run }
      subroutine test_lower
      implicit none
      character(3), dimension(3) :: zsymel,zsymelr
      common /xx/ zsymel, zsymelr
      integer :: znsymelr
      zsymel = (/ 'X', 'Y', ' ' /)
      zsymelr= (/ 'X', 'Y', ' ' /)
      znsymelr=2
      call check_zsymel(zsymel,zsymelr,znsymelr)

      contains

      subroutine check_zsymel(zsymel,zsymelr,znsymelr)
        implicit none
        integer znsymelr, isym
        character(*) zsymel(*),zsymelr(*)
        character(len=80) buf
        zsymel(3)(lenstr(zsymel(3))+1:)='X'
        write (buf,10) (trim(zsymelr(isym)),isym=1,znsymelr)
10      format(3(a,:,','))
        if (trim(buf) /= 'X,Y') call abort
      end subroutine check_zsymel

      function lenstr(s)
        character(len=*),intent(in) :: s
        integer :: lenstr
        if (len_trim(s) /= 0) call abort
        lenstr = len_trim(s)
      end function lenstr

      end subroutine test_lower

      subroutine test_upper
      implicit none
      character(3), dimension(3) :: zsymel,zsymelr
      common /xx/ zsymel, zsymelr
      integer :: znsymelr
      zsymel = (/ 'X', 'Y', ' ' /)
      zsymelr= (/ 'X', 'Y', ' ' /)
      znsymelr=2
      call check_zsymel(zsymel,zsymelr,znsymelr)

      contains

      subroutine check_zsymel(zsymel,zsymelr,znsymelr)
        implicit none
        integer znsymelr, isym
        character(*) zsymel(*),zsymelr(*)
        character(len=80) buf
        zsymel(3)(:lenstr(zsymel(3))+1)='X'
        write (buf,20) (trim(zsymelr(isym)),isym=1,znsymelr)
20      format(3(a,:,','))
        if (trim(buf) /= 'X,Y') call abort
      end subroutine check_zsymel

      function lenstr(s)
        character(len=*),intent(in) :: s
        integer :: lenstr
        if (len_trim(s) /= 0) call abort
        lenstr = len_trim(s)
      end function lenstr

      end subroutine test_upper

      program test
        call test_lower
        call test_upper
      end program test

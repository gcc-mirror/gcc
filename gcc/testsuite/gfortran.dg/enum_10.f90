! { dg-do run }
! { dg-additional-sources enum_10.c }
! { dg-options "-fshort-enums -w" }
! { dg-options "-fshort-enums -w -Wl,--no-enum-size-warning" { target arm*-*-linux*eabi } }
! Make sure short enums are indeed interoperable with the
! corresponding C type.

module enum_10
enum, bind( c )
   enumerator :: one1 = 1, two1, max1 = huge(1_1)
end enum

enum, bind( c )
   enumerator :: one2 = 1, two2, max2 = huge(1_2)
end enum

enum, bind( c )
   enumerator :: one4 = 1, two4, max4 = huge(1_4)
end enum
end module enum_10

use enum_10

interface f1
  subroutine f1(i,j)
    use enum_10
    integer (kind(max1)) :: i
    integer :: j
  end subroutine f1
end interface


interface f2
  subroutine f2(i,j)
    use enum_10
    integer (kind(max2)) :: i
    integer :: j
  end subroutine f2
end interface


interface f4
  subroutine f4(i,j)
    use enum_10
    integer (kind(max4)) :: i
    integer :: j
  end subroutine f4
end interface


call f1 (one1, 1)
call f1 (two1, 2)
call f1 (max1, huge(1_1)+0) ! Adding 0 to get default integer

call f2 (one2, 1)
call f2 (two2, 2)
call f2 (max2, huge(1_2)+0)

call f4 (one4, 1)
call f4 (two4, 2)
call f4 (max4, huge(1_4)+0)
end

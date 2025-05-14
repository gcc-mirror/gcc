! { dg-do run }
! { dg-additional-options "-fdump-tree-original" }

module m
implicit none
contains
subroutine sub(y,str)
integer :: y, x, i
character(len=5) :: str
character(len=5) :: z = "abcde"
logical :: error = .false.

x = 5
z = "12345"
do concurrent (i = 1: 3) local_init(x) local_init(z) shared(error)default(none)
   if (x /= 5) error = .true.
   if (z /= "12345") error = .true.
   x =  99
   z = "XXXXX"
end do
if (x /= 5 .or. z /= "12345") stop 1
if (error) stop 2

do concurrent (i = 1: 3) local(y) local(str) shared(error) default(none)
   y =  99
   str = "XXXXX"
end do
if (y /= 42 .or. str /= "ABCDE") stop 3
end
end

use m
implicit none
character(len=5) :: chars = "ABCDE"
integer :: fourtytwo = 42
call sub(fourtytwo, chars)
end


! { dg-final { scan-tree-dump-times "  integer\\(kind=4\\) x;" 2 "original" } }
! { dg-final { scan-tree-dump-times "  static character\\(kind=1\\) z\\\[1:5\\\] = .abcde.;" 1 "original" } }
! { dg-final { scan-tree-dump-times "  character\\(kind=1\\) z\\\[1:5\\\];" 1 "original" } }
! { dg-final { scan-tree-dump-times "  integer\\(kind=4\\) y;" 1 "original" } }
! { dg-final { scan-tree-dump-times "  character\\(kind=1\\) str\\\[1:5\\\];" 1 "original" } }

! { dg-final { scan-tree-dump-times "  x = 5;" 1 "original" } }
! { dg-final { scan-tree-dump-times "  __builtin_memmove \\(\\(void \\*\\) &z, \\(void \\*\\) &.12345.\\\[1\\\]\{lb: 1 sz: 1\}, 5\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "  x = x;" 1 "original" } }
! { dg-final { scan-tree-dump-times "  __builtin_memmove \\(\\(void \\*\\) &z, \\(void \\*\\)\\ &z, 5\\);" 1 "original" } }

! { dg-final { scan-tree-dump-not "  y = y;" "original" } }
! { dg-final { scan-tree-dump-times "  __builtin_memmove \\(\\(void \\*\\) &str, \\(void \\*\\)\\ &.XXXXX.\\\[1\\\]\{lb: 1 sz: 1\}, 5\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "  __builtin_memmove \\(\\(void \\*\\) &str," 1 "original" } }

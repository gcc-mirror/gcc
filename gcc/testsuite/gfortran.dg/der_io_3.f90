! PR23843
! Make sure derived type I/O with PRIVATE components works where it's allowed
module m1
  type t1
     integer i
  end type t1
end module m1

module m2
  use m1

  type t2
     private
     type (t1) t
  end type t2

  type t3
     private
     integer i
  end type t3

contains
  subroutine test
    character*20 c
    type(t2) :: a
    type(t3) :: b

    a % t % i = 31337
    b % i = 255
    
    write(c,*) a
    if (trim(adjustl(c)) /= "31337") call abort
    write(c,*) b
    if (trim(adjustl(c)) /= "255") call abort
  end subroutine test
end module m2

use m2
call test
end

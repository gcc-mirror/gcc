! { dg-do compile }
module A
  use, intrinsic :: iso_c_binding
contains
  subroutine pA() bind(c, name='printf') ! { dg-error "collides" }
    print *, 'hello from pA'
  end subroutine pA
end module A

module B
  use, intrinsic :: iso_c_binding

contains
  subroutine pB() bind(c, name='printf') ! { dg-error "collides" }
    print *, 'hello from pB'
  end subroutine pB
end module B

module C
use A
use B ! { dg-error "Can't open module file" }
end module C



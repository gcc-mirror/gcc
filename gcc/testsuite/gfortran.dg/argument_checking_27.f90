! { dg-do compile }
! { dg-additional-options "-std=f2018 -Wcharacter-truncation" }
! PR fortran/93330
!
! Exercise compile-time checking of character length of dummy vs.
! actual arguments.  Based on original testcase by Tobias Burnus

module m
  use iso_c_binding, only: c_char
  implicit none
contains
  ! scalar dummy
  ! character(kind=1):
  subroutine zero(x, y)
    character(kind=1,len=0), value :: x
    character(kind=1,len=1), value :: y
    print '(5a)', 'zero >', x, '< >', y, '<'
  end
  subroutine one(x, y)
    character(kind=1,len=1), value :: x
    character(kind=1,len=1), value :: y
    print '(5a)','one >', x, '< >', y, '<'
  end
  subroutine two(x, y)
    character(kind=1,len=2), value :: x
    character(kind=1,len=1), value :: y
    print '(5a)','two >', x, '< >', y, '<'
  end
  subroutine cbind(x, y) bind(C)
    character(kind=c_char,len=1), value :: x
    character(kind=c_char,len=1), value :: y
    print '(5a)','cbind >', x, '< >', y, '<'
  end

  ! character(kind=4):
  subroutine zero4(x, y)
    character(kind=4,len=0), value :: x
    character(kind=1,len=1), value :: y
    print '(5a)', 'zero4 >', x, '< >', y, '<'
  end
  subroutine one4(x, y)
    character(kind=4,len=1), value :: x
    character(kind=1,len=1), value :: y
    print '(5a)','one4 >', x, '< >', y, '<'
  end
  subroutine two4(x, y)
    character(kind=4,len=2), value :: x
    character(kind=1,len=1), value :: y
    print '(5a)','two4 >', x, '< >', y, '<'
  end

  ! character(kind=1):
  ! array dummy, assumed size
  subroutine zero_0(x, y)
    character(kind=1,len=0) :: x(*)
    character(kind=1,len=1), value :: y
    print '(5a)', 'zero_0 >', x(1), '< >', y, '<'
  end
  subroutine one_0(x, y)
    character(kind=1,len=1) :: x(*)
    character(kind=1,len=1), value :: y
    print '(5a)','one_0 >', x(1), '< >', y, '<'
  end
  subroutine two_0(x, y)
    character(kind=1,len=2) :: x(*)
    character(kind=1,len=1), value :: y
    print '(5a)','two_0 >', x(1), '< >', y, '<'
  end

  ! array dummy, explicit size
  subroutine zero_1(x, y)
    character(kind=1,len=0) :: x(1)
    character(kind=1,len=1), value :: y
    print '(5a)', 'zero_1 >', x(1), '< >', y, '<'
  end
  subroutine one_1(x, y)
    character(kind=1,len=1) :: x(1)
    character(kind=1,len=1), value :: y
    print '(5a)','one_1 >', x(1), '< >', y, '<'
  end
  subroutine two_1(x, y)
    character(kind=1,len=2) :: x(1)
    character(kind=1,len=1), value :: y
    print '(5a)','two_1 >', x(1), '< >', y, '<'
  end

  ! array dummy, assumed shape
  subroutine zero_a(x, y)
    character(kind=1,len=0) :: x(:)
    character(kind=1,len=1), value :: y
    if (size (x) < 1) stop 99
    print '(5a)', 'zero_a >', x(1), '< >', y, '<'
  end
  subroutine one_a(x, y)
    character(kind=1,len=1) :: x(:)
    character(kind=1,len=1), value :: y
    if (size (x) < 1) stop 99
    print '(5a)','one_a >', x(1), '< >', y, '<'
  end
  subroutine two_a(x, y)
    character(kind=1,len=2) :: x(:)
    character(kind=1,len=1), value :: y
    if (size (x) < 1) stop 99
    print '(5a)','two_a >', x(1), '< >', y, '<'
  end

  ! character(kind=4):
  ! array dummy, assumed size
  subroutine zero4_0(x, y)
    character(kind=4,len=0) :: x(*)
    character(kind=4,len=1), value :: y
    print '(5a)', 'zero4_0 >', x(1), '< >', y, '<'
  end
  subroutine one4_0(x, y)
    character(kind=4,len=1) :: x(*)
    character(kind=4,len=1), value :: y
    print '(5a)','one4_0 >', x(1), '< >', y, '<'
  end
  subroutine two4_0(x, y)
    character(kind=4,len=2) :: x(*)
    character(kind=4,len=1), value :: y
    print '(5a)','two4_0 >', x(1), '< >', y, '<'
  end

  ! array dummy, explicit size
  subroutine zero4_1(x, y)
    character(kind=4,len=0) :: x(1)
    character(kind=4,len=1), value :: y
    print '(5a)', 'zero4_1 >', x(1), '< >', y, '<'
  end
  subroutine one4_1(x, y)
    character(kind=4,len=1) :: x(1)
    character(kind=4,len=1), value :: y
    print '(5a)','one4_1 >', x(1), '< >', y, '<'
  end
  subroutine two4_1(x, y)
    character(kind=4,len=2) :: x(1)
    character(kind=4,len=1), value :: y
    print '(5a)','two4_1 >', x(1), '< >', y, '<'
  end

  ! array dummy, assumed shape
  subroutine zero4_a(x, y)
    character(kind=4,len=0) :: x(:)
    character(kind=4,len=1), value :: y
    if (size (x) < 1) stop 99
    print '(5a)', 'zero4_a >', x(1), '< >', y, '<'
  end
  subroutine one4_a(x, y)
    character(kind=4,len=1) :: x(:)
    character(kind=4,len=1), value :: y
    if (size (x) < 1) stop 99
    print '(5a)','one4_a >', x(1), '< >', y, '<'
  end
  subroutine two4_a(x, y)
    character(kind=4,len=2) :: x(:)
    character(kind=4,len=1), value :: y
    if (size (x) < 1) stop 99
    print '(5a)','two4_a >', x(1), '< >', y, '<'
  end
end

program p
  use m
  implicit none
  call zero('', '1')
  call one ('', '2')      ! { dg-error "length of actual argument shorter" }
  call one ('b'(3:2),'3') ! { dg-error "length of actual argument shorter" }
  call two ('', '4')      ! { dg-error "length of actual argument shorter" }
  call two ('f','5')      ! { dg-error "length of actual argument shorter" }

  call cbind('',   '6')   ! { dg-error "length of actual argument shorter" }
  call cbind('ABC','7')   ! { dg-warning "length of actual argument longer" }

  ! character(kind=4):
  call zero4(4_'', '8')
  call zero4(4_'3','9')      ! { dg-warning "length of actual argument longer" }
  call one4 (4_'', 'A')      ! { dg-error "length of actual argument shorter" }
  call one4 (4_'b'(3:2),'B') ! { dg-error "length of actual argument shorter" }
  call one4 (4_'bbcd'(3:3),'C')
  call one4 (4_'cd','D')     ! { dg-warning "length of actual argument longer" }
  call two4 (4_'',  'E')     ! { dg-error "length of actual argument shorter" }
  call two4 (4_'f', 'F')     ! { dg-error "length of actual argument shorter" }
  call two4 (4_'fgh','G')    ! { dg-warning "length of actual argument longer" }

  ! array dummy, assumed size
  call zero_0([''],'a')
  call zero_0(['a'],'b')
  call one_0 ([''],'c')
  call one_0 (['b'],'d')
  call one_0 (['cd'],'e')
  call two_0 ([''],'f')
  call two_0 (['fg'],'g')

  ! array dummy, explicit size
  call zero_1([''],'a')
  call zero_1(['a'],'b')  ! { dg-warning "actual argument longer" }
  call one_1 ([''],'c')   ! { dg-error "too few elements for dummy" }
  call one_1 (['b'],'d')
  call one_1 (['cd'],'e') ! { dg-warning "actual argument longer" }
  call two_1 ([''],'f')   ! { dg-error "too few elements for dummy" }
  call two_1 (['fg'],'h')

  ! array dummy, assumed shape
  call zero_a([''],'a')
  call zero_a(['a'],'b')  ! { dg-error "Character length mismatch" }
  call one_a ([''],'c')   ! { dg-error "Character length mismatch" }
  call one_a (['b'],'d')
  call one_a (['cd'],'e') ! { dg-error "Character length mismatch" }
  call two_a ([''],'f')   ! { dg-error "Character length mismatch" }
  call two_a (['fg'],'h')

  ! character(kind=4):
  ! array dummy, assumed size
  call zero4_0([4_''],4_'a')
  call zero4_0([4_'a'],4_'b')
  call one4_0 ([4_''],4_'c')
  call one4_0 ([4_'b'],4_'d')
  call one4_0 ([4_'cd'],4_'e')
  call two4_0 ([4_''],4_'f')
  call two4_0 ([4_'fg'],4_'g')

  ! array dummy, explicit size
  call zero4_1([4_''],4_'a')
  call zero4_1([4_'a'],4_'b')  ! { dg-warning "actual argument longer" }
  call one4_1 ([4_''],4_'c')   ! { dg-error "too few elements for dummy" }
  call one4_1 ([4_'b'],4_'d')
  call one4_1 ([4_'cd'],4_'e') ! { dg-warning "actual argument longer" }
  call two4_1 ([4_''],4_'f')   ! { dg-error "too few elements for dummy" }
  call two4_1 ([4_'fg'],4_'h')

  ! array dummy, assumed shape
  call zero4_a([4_''],4_'a')
  call zero4_a([4_'a'],4_'b')  ! { dg-error "Character length mismatch" }
  call one4_a ([4_''],4_'c')   ! { dg-error "Character length mismatch" }
  call one4_a ([4_'b'],4_'d')
  call one4_a ([4_'cd'],4_'e') ! { dg-error "Character length mismatch" }
  call two4_a ([4_''],4_'f')   ! { dg-error "Character length mismatch" }
  call two4_a ([4_'fg'],4_'h')
end

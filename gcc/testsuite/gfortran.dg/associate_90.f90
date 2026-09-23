! { dg-do run }
!
! PR 127187
! %LEN and %KIND inquiries of deferred-length character in ASSOCIATE.
!
! Contributed by Ivan Pribec
!
program assoc_len_kind
  implicit none
  character(:), allocatable :: s, a(:)
  character(:), pointer :: p
  character(kind=4,len=:), allocatable :: w
  character(7) :: f = 'seven07'
  complex :: z = (1., 2.)
  s = 'hello'
  a = ['ab', 'cd', 'ef']
  w = 4_'wxyz'
  allocate (p, source='ptr')

  ! Selector contains an inquiry of a deferred-length variable
  associate (n => s%len)
    if (n /= 5) error stop 1
  end associate
  associate (n => (s%len))
    if (n /= 5) error stop 2
  end associate
  associate (n => f%len)
    if (n /= 7) error stop 3
  end associate
  associate (n => s%len + 0)
    if (n /= 5) error stop 4
  end associate
  associate (k => z%kind)
    if (k /= kind(z)) error stop 5
  end associate
  associate (k => z%kind, n => s%len)
    if (k /= kind(z)) error stop 6
    if (n /= 5) error stop 7
  end associate
  associate (n => s%len)
    s = 'longer_string'
    if (n /= 5) error stop 8
  end associate
  s = 'hello'
  associate (n => s%kind, m => p%len, k => w%kind)
    if (n /= 1 .or. m /= 3 .or. k /= 4) error stop 9
  end associate

  ! Inquiry of an associate-name with a deferred-length selector
  associate (q => s)
    if (q%len /= 5 .or. q%kind /= 1) error stop 10
    if (q(2:3)%len /= 2) error stop 11
    associate (n => q%len + q%kind)
      if (n /= 6) error stop 12
    end associate
  end associate
  associate (q => a)
    if (q%len /= 2 .or. size (q) /= 3) error stop 13
  end associate
  associate (q => p, r => w)
    if (q%len /= 3 .or. r%len /= 4 .or. r%kind /= 4) error stop 14
  end associate
  associate (q => s)
    associate (r => q)
      if (r%len /= 5) error stop 15
    end associate
  end associate
  deallocate (p)
end program

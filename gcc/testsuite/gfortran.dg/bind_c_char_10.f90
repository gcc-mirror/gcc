! { dg-do run }
! { dg-additional-options "-fdump-tree-original" }

! F2018  - examples with array descriptor

module m
  use iso_c_binding, only: c_char
  implicit none (type, external)

contains

! Assumed-shape array, nonallocatable/nonpointer

subroutine as1 (x1) bind(C)
  character(kind=c_char, len=1) :: x1(:)
  if (size(x1) /= 6) stop
  if (len(x1) /= 1) stop  
  if (any (x1 /= ['g', &
                  'd', &
                  'f', &
                  's', &
                  '3', &
                  '5'])) stop 1
  x1 = ['1', &
        'h', &
        'f', &
        '3', &
        '4', &
        'h']
end

subroutine as2 (x2) bind(C)
  character(kind=c_char, len=2) :: x2(:)
  if (size(x2) /= 6) stop
  if (len(x2) /= 2) stop  
  if (any (x2 /= ['ab', &
                  'fd', &
                  'D4', &
                  '54', &
                  'ga', &
                  'hg'])) stop
  x2 = ['ab', &
        'hd', &
        'fj', &
        'a4', &
        '4a', &
        'hf']
end

subroutine as3 (xn, n) bind(C)
  integer :: n
  character(kind=c_char, len=n) :: xn(:)
  if (size(xn) /= 6) stop
  if (len(xn) /= 5) stop  
  if (any (xn /= ['DDGhf', &
                 'hdrh$', &
                 'fDGSl', &
                 'DFHs3', &
                 '43grG', &
                 'hFG$k'])) stop
  xn = ['FDGhf', &
       'hdrhg', &
       'fDgFl', &
       'DFHs3', &
       '4a54G', &
       'hSs6k']
end

subroutine as4 (xstar) bind(C)
  character(kind=c_char, len=*) :: xstar(:)
  if (size(xstar) /= 6) stop
  if (len(xstar) /= 5) stop  
  if (any (xstar /= ['DDGhf', &
                     'hdrh$', &
                     'fDGSl', &
                     'DFHs3', &
                     '43grG', &
                     'hFG$k'])) stop
  xstar = ['FDGhf', &
           'hdrhg', &
           'fDgFl', &
           'DFHs3', &
           '4a54G', &
           'hSs6k']
end

! Assumed-rank array, nonallocatable/nonpointer

subroutine ar1 (x1) bind(C)
  character(kind=c_char, len=1) :: x1(..)
  if (size(x1) /= 6) stop
  if (len(x1) /= 1) stop  
  select rank(x1)
    rank(1)
      if (any (x1 /= ['g', &
                      'd', &
                      'f', &
                      's', &
                      '3', &
                      '5'])) stop
      x1 = ['1', &
            'h', &
            'f', &
            '3', &
            '4', &
            'h']
  rank default
    stop
  end select
end

subroutine ar2 (x2) bind(C)
  character(kind=c_char, len=2) :: x2(..)
  if (size(x2) /= 6) stop
  if (len(x2) /= 2) stop  
  select rank(x2)
    rank(1)
      if (any (x2 /= ['ab', &
                      'fd', &
                      'D4', &
                      '54', &
                      'ga', &
                      'hg'])) stop
      x2 = ['ab', &
            'hd', &
            'fj', &
            'a4', &
            '4a', &
            'hf']
    rank default
      stop
  end select
end

subroutine ar3 (xn, n) bind(C)
  integer :: n
  character(len=n) :: xn(..)
  if (size(xn) /= 6) stop
  if (len(xn) /= 5) stop  
  select rank(xn)
    rank(1)
      if (any (xn /= ['DDGhf', &
                      'hdrh$', &
                      'fDGSl', &
                      'DFHs3', &
                      '43grG', &
                      'hFG$k'])) stop
      xn = ['FDGhf', &
            'hdrhg', &
            'fDgFl', &
            'DFHs3', &
            '4a54G', &
            'hSs6k']
  rank default
    stop
  end select
end

subroutine ar4 (xstar) bind(C)
  character(kind=c_char, len=*) :: xstar(..)
  if (size(xstar) /= 6) stop
  if (len(xstar) /= 5) stop  
  select rank(xstar)
    rank(1)
      if (any (xstar /= ['DDGhf', &
                         'hdrh$', &
                         'fDGSl', &
                         'DFHs3', &
                         '43grG', &
                         'hFG$k'])) stop
      xstar = ['FDGhf', &
               'hdrhg', &
               'fDgFl', &
               'DFHs3', &
               '4a54G', &
               'hSs6k']
  rank default
    stop
  end select
end

! ALLOCATABLE

! Assumed-shape array, allocatable

subroutine a5a (xcolon) bind(C)
  character(kind=c_char, len=:), allocatable :: xcolon(:)
  if (.not. allocated (xcolon)) stop
  if (size(xcolon) /= 6) stop
  if (len(xcolon) /= 5) stop  
  if (any (xcolon /= ['DDGhf', &
                     'hdrh$', &
                     'fDGSl', &
                     'DFHs3', &
                     '43grG', &
                     'hFG$k'])) stop
  xcolon = ['FDGhf', &
           'hdrhg', &
           'fDgFl', &
           'DFHs3', &
           '4a54G', &
           'hSs6k']
end

! Assumed-rank array, allocatable

subroutine a5ar (xcolon) bind(C)
  character(kind=c_char, len=:), allocatable :: xcolon(..)
  if (.not. allocated (xcolon)) stop
  if (size(xcolon) /= 6) stop
  if (len(xcolon) /= 5) stop  
  select rank(xcolon)
    rank(1)
      if (any (xcolon /= ['DDGhf', &
                         'hdrh$', &
                         'fDGSl', &
                         'DFHs3', &
                         '43grG', &
                         'hFG$k'])) stop
      xcolon = ['FDGhf', &
               'hdrhg', &
               'fDgFl', &
               'DFHs3', &
               '4a54G', &
               'hSs6k']
  rank default
    stop
  end select
end

! POINTER
! Assumed-shape array, pointer

subroutine a5p (xcolon) bind(C)
  character(kind=c_char, len=:), pointer :: xcolon(:)
  if (.not. associated (xcolon)) stop
  if (size(xcolon) /= 6) stop
  if (len(xcolon) /= 5) stop  
  if (any (xcolon /= ['DDGhf', &
                     'hdrh$', &
                     'fDGSl', &
                     'DFHs3', &
                     '43grG', &
                     'hFG$k'])) stop
  xcolon = ['FDGhf', &
           'hdrhg', &
           'fDgFl', &
           'DFHs3', &
           '4a54G', &
           'hSs6k']
end

! Assumed-rank array, pointer

subroutine a5pr (xcolon) bind(C)
  character(kind=c_char, len=:), pointer :: xcolon(..)
  if (.not. associated (xcolon)) stop
  if (size(xcolon) /= 6) stop
  if (len(xcolon) /= 5) stop  
  select rank(xcolon)
    rank(1)
      if (any (xcolon /= ['DDGhf', &
                         'hdrh$', &
                         'fDGSl', &
                         'DFHs3', &
                         '43grG', &
                         'hFG$k'])) stop
      xcolon = ['FDGhf', &
               'hdrhg', &
               'fDgFl', &
               'DFHs3', &
               '4a54G', &
               'hSs6k']
  rank default
    stop
  end select
end
end module m

program main
  use m
  implicit none (type, external)
  character(kind=c_char, len=1) :: str1a6(6)
  character(kind=c_char, len=2) :: str2a6(6)
  character(kind=c_char, len=5) :: str5a6(6)

  character(kind=c_char, len=:), allocatable :: astr5a6(:)
  character(kind=c_char, len=:), pointer :: pstr5a6(:)

  allocate (character(kind=c_char, len=5) :: astr5a6(6), pstr5a6(6))

  ! assumed shape - with array descriptor

  str1a6 = ['g', &
            'd', &
            'f', &
            's', &
            '3', &
            '5']
  call as1 (str1a6)
  if (any (str1a6 /= ['1', &
                      'h', &
                      'f', &
                      '3', &
                      '4', &
                      'h'])) stop
  str2a6 = ['ab', &
            'fd', &
            'D4', &
            '54', &
            'ga', &
            'hg']
  call as2 (str2a6)
  if (any (str2a6 /= ['ab', &
                      'hd', &
                      'fj', &
                      'a4', &
                      '4a', &
                      'hf'])) stop

  str5a6 = ['DDGhf', &
            'hdrh$', &
            'fDGSl', &
            'DFHs3', &
            '43grG', &
            'hFG$k']
  call as3 (str5a6, 5)
  if (any (str5a6 /= ['FDGhf', &
                      'hdrhg', &
                      'fDgFl', &
                      'DFHs3', &
                      '4a54G', &
                      'hSs6k'])) stop

  str5a6 = ['DDGhf', &
            'hdrh$', &
            'fDGSl', &
            'DFHs3', &
            '43grG', &
            'hFG$k']
  call as4 (str5a6)
  if (any (str5a6 /= ['FDGhf', &
                      'hdrhg', &
                      'fDgFl', &
                      'DFHs3', &
                      '4a54G', &
                      'hSs6k'])) stop

  ! assumed rank - with array descriptor

  str1a6 = ['g', &
            'd', &
            'f', &
            's', &
            '3', &
            '5']
  call ar1 (str1a6)
  if (any (str1a6 /= ['1', &
                      'h', &
                      'f', &
                      '3', &
                      '4', &
                      'h'])) stop
  str2a6 = ['ab', &
            'fd', &
            'D4', &
            '54', &
            'ga', &
            'hg']
  call ar2 (str2a6)
  if (any (str2a6 /= ['ab', &
                      'hd', &
                      'fj', &
                      'a4', &
                      '4a', &
                      'hf'])) stop

  str5a6 = ['DDGhf', &
            'hdrh$', &
            'fDGSl', &
            'DFHs3', &
            '43grG', &
            'hFG$k']
  call ar3 (str5a6, 5)
  if (any (str5a6 /= ['FDGhf', &
                      'hdrhg', &
                      'fDgFl', &
                      'DFHs3', &
                      '4a54G', &
                      'hSs6k'])) stop


  str5a6 = ['DDGhf', &
            'hdrh$', &
            'fDGSl', &
            'DFHs3', &
            '43grG', &
            'hFG$k']
  call ar4 (str5a6)
  if (any (str5a6 /= ['FDGhf', &
                      'hdrhg', &
                      'fDgFl', &
                      'DFHs3', &
                      '4a54G', &
                      'hSs6k'])) stop

  ! allocatable - with array descriptor
  astr5a6(:) = ['DDGhf', &
                'hdrh$', &
                'fDGSl', &
                'DFHs3', &
                '43grG', &
                'hFG$k']
  call a5a (astr5a6)
  if (any (astr5a6 /= ['FDGhf', &
                       'hdrhg', &
                       'fDgFl', &
                       'DFHs3', &
                       '4a54G', &
                       'hSs6k'])) stop

  astr5a6(:) = ['DDGhf', &
                'hdrh$', &
                'fDGSl', &
                'DFHs3', &
                '43grG', &
                'hFG$k']
  call a5ar (astr5a6)
  if (any (astr5a6 /= ['FDGhf', &
                       'hdrhg', &
                       'fDgFl', &
                       'DFHs3', &
                       '4a54G', &
                       'hSs6k'])) stop


  ! pointer - with array descriptor
  pstr5a6 = ['DDGhf', &
             'hdrh$', &
             'fDGSl', &
             'DFHs3', &
             '43grG', &
             'hFG$k']
  call a5p (pstr5a6)
  if (any (pstr5a6 /= ['FDGhf', &
                       'hdrhg', &
                       'fDgFl', &
                       'DFHs3', &
                       '4a54G', &
                       'hSs6k'])) stop

  pstr5a6 = ['DDGhf', &
             'hdrh$', &
             'fDGSl', &
             'DFHs3', &
             '43grG', &
             'hFG$k']
  call a5pr (pstr5a6)
  if (any (pstr5a6 /= ['FDGhf', &
                       'hdrhg', &
                       'fDgFl', &
                       'DFHs3', &
                       '4a54G', &
                       'hSs6k'])) stop
  deallocate (astr5a6, pstr5a6)
end

! All arguments shall use array descriptors
! { dg-final { scan-tree-dump-times "void as1 \\(struct CFI_cdesc_t01 & restrict _x1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "void as2 \\(struct CFI_cdesc_t01 & restrict _x2\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "void as3 \\(struct CFI_cdesc_t01 & restrict _xn, integer\\(kind=4\\) & restrict n\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "void as4 \\(struct CFI_cdesc_t01 & restrict _xstar\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "void ar1 \\(struct CFI_cdesc_t & restrict _x1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "void ar2 \\(struct CFI_cdesc_t & restrict _x2\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "void ar3 \\(struct CFI_cdesc_t & restrict _xn, integer\\(kind=4\\) & restrict n\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "void ar4 \\(struct CFI_cdesc_t & restrict _xstar\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "void a5ar \\(struct CFI_cdesc_t & restrict _xcolon\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "void a5a \\(struct CFI_cdesc_t01 & restrict _xcolon\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "void a5pr \\(struct CFI_cdesc_t & _xcolon\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "void a5p \\(struct CFI_cdesc_t01 & _xcolon\\)" 1 "original" } }


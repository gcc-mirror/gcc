! { dg-do run }
!
! PR fortran/112371
! The library used to incorrectly set an extent of zero for the first
! dimension of the resulting array of a reduction function if that array was
! empty.

program p
  implicit none
  call check_iparity
  call check_sum
  call check_minloc_int
  call check_minloc_char
  call check_maxloc_char4
  call check_minval_char
  call check_maxval_char4
  call check_any
  call check_count4
  call check_findloc_int
  call check_findloc_char
contains
  subroutine check_iparity
    integer :: a(9,3,0,7)
    logical :: m1(9,3,0,7)
    logical(kind=4) :: m4
    integer :: i
    integer, allocatable :: r(:,:,:)
    a  = reshape((/ integer:: /), shape(a))
    m1 = reshape((/ logical:: /), shape(m1))
    m4 = .false.
    i = 1
    r = iparity(a, dim=i)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 111
    if (any(ubound(r) /= (/ 3, 0, 7 /))) stop 112
    i = 2
    r = iparity(a, dim=i)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 113
    if (any(ubound(r) /= (/ 9, 0, 7 /))) stop 114
    i = 3
    r = iparity(a, dim=i)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 115
    if (any(ubound(r) /= (/ 9, 3, 7 /))) stop 116
    i = 4
    r = iparity(a, dim=i)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 117
    if (any(ubound(r) /= (/ 9, 3, 0 /))) stop 118
    i = 1
    r = iparity(a, dim=i, mask=m1)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 121
    if (any(ubound(r) /= (/ 3, 0, 7 /))) stop 122
    i = 2
    r = iparity(a, dim=i, mask=m1)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 123
    if (any(ubound(r) /= (/ 9, 0, 7 /))) stop 124
    i = 3
    r = iparity(a, dim=i, mask=m1)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 125
    if (any(ubound(r) /= (/ 9, 3, 7 /))) stop 126
    i = 4
    r = iparity(a, dim=i, mask=m1)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 127
    if (any(ubound(r) /= (/ 9, 3, 0 /))) stop 128
    i = 1
    r = iparity(a, dim=i, mask=m4)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 131
    if (any(ubound(r) /= (/ 3, 0, 7 /))) stop 132
    i = 2
    r = iparity(a, dim=i, mask=m4)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 133
    if (any(ubound(r) /= (/ 9, 0, 7 /))) stop 134
    i = 3
    r = iparity(a, dim=i, mask=m4)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 135
    if (any(ubound(r) /= (/ 9, 3, 7 /))) stop 136
    i = 4
    r = iparity(a, dim=i, mask=m4)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 137
    if (any(ubound(r) /= (/ 9, 3, 0 /))) stop 138
  end subroutine
  subroutine check_sum
    integer :: a(9,3,0,7)
    logical :: m1(9,3,0,7)
    logical(kind=4) :: m4
    integer :: i
    integer, allocatable :: r(:,:,:)
    a  = reshape((/ integer:: /), shape(a))
    m1 = reshape((/ logical:: /), shape(m1))
    m4 = .false.
    i = 1
    r = sum(a, dim=i)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 211
    if (any(ubound(r) /= (/ 3, 0, 7 /))) stop 212
    i = 2
    r = sum(a, dim=i)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 213
    if (any(ubound(r) /= (/ 9, 0, 7 /))) stop 214
    i = 3
    r = sum(a, dim=i)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 215
    if (any(ubound(r) /= (/ 9, 3, 7 /))) stop 216
    i = 4
    r = sum(a, dim=i)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 217
    if (any(ubound(r) /= (/ 9, 3, 0 /))) stop 218
    i = 1
    r = sum(a, dim=i, mask=m1)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 221
    if (any(ubound(r) /= (/ 3, 0, 7 /))) stop 222
    i = 2
    r = sum(a, dim=i, mask=m1)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 223
    if (any(ubound(r) /= (/ 9, 0, 7 /))) stop 224
    i = 3
    r = sum(a, dim=i, mask=m1)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 225
    if (any(ubound(r) /= (/ 9, 3, 7 /))) stop 226
    i = 4
    r = sum(a, dim=i, mask=m1)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 227
    if (any(ubound(r) /= (/ 9, 3, 0 /))) stop 228
    i = 1
    r = sum(a, dim=i, mask=m4)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 231
    if (any(ubound(r) /= (/ 3, 0, 7 /))) stop 232
    i = 2
    r = sum(a, dim=i, mask=m4)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 233
    if (any(ubound(r) /= (/ 9, 0, 7 /))) stop 234
    i = 3
    r = sum(a, dim=i, mask=m4)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 235
    if (any(ubound(r) /= (/ 9, 3, 7 /))) stop 236
    i = 4
    r = sum(a, dim=i, mask=m4)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 237
    if (any(ubound(r) /= (/ 9, 3, 0 /))) stop 238
  end subroutine
  subroutine check_minloc_int
    integer :: a(9,3,0,7)
    logical :: m1(9,3,0,7)
    logical(kind=4) :: m4
    integer :: i
    integer, allocatable :: r(:,:,:)
    a  = reshape((/ integer:: /), shape(a))
    m1 = reshape((/ logical:: /), shape(m1))
    m4 = .false.
    i = 1
    r = minloc(a, dim=i)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 311
    if (any(ubound(r) /= (/ 3, 0, 7 /))) stop 312
    i = 2
    r = minloc(a, dim=i)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 313
    if (any(ubound(r) /= (/ 9, 0, 7 /))) stop 314
    i = 3
    r = minloc(a, dim=i)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 315
    if (any(ubound(r) /= (/ 9, 3, 7 /))) stop 316
    i = 4
    r = minloc(a, dim=i)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 317
    if (any(ubound(r) /= (/ 9, 3, 0 /))) stop 318
    i = 1
    r = minloc(a, dim=i, mask=m1)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 321
    if (any(ubound(r) /= (/ 3, 0, 7 /))) stop 322
    i = 2
    r = minloc(a, dim=i, mask=m1)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 323
    if (any(ubound(r) /= (/ 9, 0, 7 /))) stop 324
    i = 3
    r = minloc(a, dim=i, mask=m1)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 325
    if (any(ubound(r) /= (/ 9, 3, 7 /))) stop 326
    i = 4
    r = minloc(a, dim=i, mask=m1)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 327
    if (any(ubound(r) /= (/ 9, 3, 0 /))) stop 328
    i = 1
    r = minloc(a, dim=i, mask=m4)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 331
    if (any(ubound(r) /= (/ 3, 0, 7 /))) stop 332
    i = 2
    r = minloc(a, dim=i, mask=m4)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 333
    if (any(ubound(r) /= (/ 9, 0, 7 /))) stop 334
    i = 3
    r = minloc(a, dim=i, mask=m4)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 335
    if (any(ubound(r) /= (/ 9, 3, 7 /))) stop 336
    i = 4
    r = minloc(a, dim=i, mask=m4)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 337
    if (any(ubound(r) /= (/ 9, 3, 0 /))) stop 338
  end subroutine
  subroutine check_minloc_char
    character :: a(9,3,0,7)
    logical :: m1(9,3,0,7)
    logical(kind=4) :: m4
    integer :: i
    integer, allocatable :: r(:,:,:)
    a  = reshape((/ character:: /), shape(a))
    m1 = reshape((/ logical:: /), shape(m1))
    m4 = .false.
    i = 1
    r = minloc(a, dim=i)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 411
    if (any(ubound(r) /= (/ 3, 0, 7 /))) stop 412
    i = 2
    r = minloc(a, dim=i)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 413
    if (any(ubound(r) /= (/ 9, 0, 7 /))) stop 414
    i = 3
    r = minloc(a, dim=i)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 415
    if (any(ubound(r) /= (/ 9, 3, 7 /))) stop 416
    i = 4
    r = minloc(a, dim=i)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 417
    if (any(ubound(r) /= (/ 9, 3, 0 /))) stop 418
    i = 1
    r = minloc(a, dim=i, mask=m1)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 421
    if (any(ubound(r) /= (/ 3, 0, 7 /))) stop 422
    i = 2
    r = minloc(a, dim=i, mask=m1)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 423
    if (any(ubound(r) /= (/ 9, 0, 7 /))) stop 424
    i = 3
    r = minloc(a, dim=i, mask=m1)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 425
    if (any(ubound(r) /= (/ 9, 3, 7 /))) stop 426
    i = 4
    r = minloc(a, dim=i, mask=m1)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 427
    if (any(ubound(r) /= (/ 9, 3, 0 /))) stop 428
    i = 1
    r = minloc(a, dim=i, mask=m4)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 431
    if (any(ubound(r) /= (/ 3, 0, 7 /))) stop 432
    i = 2
    r = minloc(a, dim=i, mask=m4)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 433
    if (any(ubound(r) /= (/ 9, 0, 7 /))) stop 434
    i = 3
    r = minloc(a, dim=i, mask=m4)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 435
    if (any(ubound(r) /= (/ 9, 3, 7 /))) stop 436
    i = 4
    r = minloc(a, dim=i, mask=m4)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 437
    if (any(ubound(r) /= (/ 9, 3, 0 /))) stop 438
  end subroutine
  subroutine check_maxloc_char4
    character(kind=4) :: a(9,3,0,7)
    logical :: m1(9,3,0,7)
    logical(kind=4) :: m4
    integer :: i
    integer, allocatable :: r(:,:,:)
    a  = reshape((/ character(kind=4):: /), shape(a))
    m1 = reshape((/ logical:: /), shape(m1))
    m4 = .false.
    i = 1
    r = maxloc(a, dim=i)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 511
    if (any(ubound(r) /= (/ 3, 0, 7 /))) stop 512
    i = 2
    r = maxloc(a, dim=i)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 513
    if (any(ubound(r) /= (/ 9, 0, 7 /))) stop 514
    i = 3
    r = maxloc(a, dim=i)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 515
    if (any(ubound(r) /= (/ 9, 3, 7 /))) stop 516
    i = 4
    r = maxloc(a, dim=i)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 517
    if (any(ubound(r) /= (/ 9, 3, 0 /))) stop 518
    i = 1
    r = maxloc(a, dim=i, mask=m1)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 521
    if (any(ubound(r) /= (/ 3, 0, 7 /))) stop 522
    i = 2
    r = maxloc(a, dim=i, mask=m1)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 523
    if (any(ubound(r) /= (/ 9, 0, 7 /))) stop 524
    i = 3
    r = maxloc(a, dim=i, mask=m1)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 525
    if (any(ubound(r) /= (/ 9, 3, 7 /))) stop 526
    i = 4
    r = maxloc(a, dim=i, mask=m1)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 527
    if (any(ubound(r) /= (/ 9, 3, 0 /))) stop 528
    i = 1
    r = maxloc(a, dim=i, mask=m4)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 531
    if (any(ubound(r) /= (/ 3, 0, 7 /))) stop 532
    i = 2
    r = maxloc(a, dim=i, mask=m4)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 533
    if (any(ubound(r) /= (/ 9, 0, 7 /))) stop 534
    i = 3
    r = maxloc(a, dim=i, mask=m4)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 535
    if (any(ubound(r) /= (/ 9, 3, 7 /))) stop 536
    i = 4
    r = maxloc(a, dim=i, mask=m4)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 537
    if (any(ubound(r) /= (/ 9, 3, 0 /))) stop 538
  end subroutine
  subroutine check_minval_char
    character :: a(9,3,0,7)
    logical :: m1(9,3,0,7)
    logical(kind=4) :: m4
    integer :: i
    character, allocatable :: r(:,:,:)
    a  = reshape((/ character:: /), shape(a))
    m1 = reshape((/ logical:: /), shape(m1))
    m4 = .false.
    i = 1
    r = minval(a, dim=i)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 611
    if (any(ubound(r) /= (/ 3, 0, 7 /))) stop 612
    i = 2
    r = minval(a, dim=i)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 613
    if (any(ubound(r) /= (/ 9, 0, 7 /))) stop 614
    i = 3
    r = minval(a, dim=i)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 615
    if (any(ubound(r) /= (/ 9, 3, 7 /))) stop 616
    i = 4
    r = minval(a, dim=i)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 617
    if (any(ubound(r) /= (/ 9, 3, 0 /))) stop 618
    i = 1
    r = minval(a, dim=i, mask=m1)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 621
    if (any(ubound(r) /= (/ 3, 0, 7 /))) stop 622
    i = 2
    r = minval(a, dim=i, mask=m1)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 623
    if (any(ubound(r) /= (/ 9, 0, 7 /))) stop 624
    i = 3
    r = minval(a, dim=i, mask=m1)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 625
    if (any(ubound(r) /= (/ 9, 3, 7 /))) stop 626
    i = 4
    r = minval(a, dim=i, mask=m1)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 627
    if (any(ubound(r) /= (/ 9, 3, 0 /))) stop 628
    i = 1
    r = minval(a, dim=i, mask=m4)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 631
    if (any(ubound(r) /= (/ 3, 0, 7 /))) stop 632
    i = 2
    r = minval(a, dim=i, mask=m4)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 633
    if (any(ubound(r) /= (/ 9, 0, 7 /))) stop 634
    i = 3
    r = minval(a, dim=i, mask=m4)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 635
    if (any(ubound(r) /= (/ 9, 3, 7 /))) stop 636
    i = 4
    r = minval(a, dim=i, mask=m4)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 637
    if (any(ubound(r) /= (/ 9, 3, 0 /))) stop 638
  end subroutine
  subroutine check_maxval_char4
    character(kind=4) :: a(9,3,0,7)
    logical :: m1(9,3,0,7)
    logical(kind=4) :: m4
    integer :: i
    character(kind=4), allocatable :: r(:,:,:)
    a  = reshape((/ character(kind=4):: /), shape(a))
    m1 = reshape((/ logical:: /), shape(m1))
    m4 = .false.
    i = 1
    r = maxval(a, dim=i)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 711
    if (any(ubound(r) /= (/ 3, 0, 7 /))) stop 712
    i = 2
    r = maxval(a, dim=i)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 713
    if (any(ubound(r) /= (/ 9, 0, 7 /))) stop 714
    i = 3
    r = maxval(a, dim=i)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 715
    if (any(ubound(r) /= (/ 9, 3, 7 /))) stop 716
    i = 4
    r = maxval(a, dim=i)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 717
    if (any(ubound(r) /= (/ 9, 3, 0 /))) stop 718
    i = 1
    r = maxval(a, dim=i, mask=m1)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 721
    if (any(ubound(r) /= (/ 3, 0, 7 /))) stop 722
    i = 2
    r = maxval(a, dim=i, mask=m1)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 723
    if (any(ubound(r) /= (/ 9, 0, 7 /))) stop 724
    i = 3
    r = maxval(a, dim=i, mask=m1)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 725
    if (any(ubound(r) /= (/ 9, 3, 7 /))) stop 726
    i = 4
    r = maxval(a, dim=i, mask=m1)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 727
    if (any(ubound(r) /= (/ 9, 3, 0 /))) stop 728
    i = 1
    r = maxval(a, dim=i, mask=m4)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 731
    if (any(ubound(r) /= (/ 3, 0, 7 /))) stop 732
    i = 2
    r = maxval(a, dim=i, mask=m4)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 733
    if (any(ubound(r) /= (/ 9, 0, 7 /))) stop 734
    i = 3
    r = maxval(a, dim=i, mask=m4)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 735
    if (any(ubound(r) /= (/ 9, 3, 7 /))) stop 736
    i = 4
    r = maxval(a, dim=i, mask=m4)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 737
    if (any(ubound(r) /= (/ 9, 3, 0 /))) stop 738
  end subroutine
  subroutine check_any
    logical :: a(9,3,0,7)
    integer :: i
    logical, allocatable :: r(:,:,:)
    a  = reshape((/ logical:: /), shape(a))
    i = 1
    r = any(a, dim=i)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 811
    if (any(ubound(r) /= (/ 3, 0, 7 /))) stop 812
    i = 2
    r = any(a, dim=i)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 813
    if (any(ubound(r) /= (/ 9, 0, 7 /))) stop 814
    i = 3
    r = any(a, dim=i)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 815
    if (any(ubound(r) /= (/ 9, 3, 7 /))) stop 816
    i = 4
    r = any(a, dim=i)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 817
    if (any(ubound(r) /= (/ 9, 3, 0 /))) stop 818
  end subroutine
  subroutine check_count4
    logical(kind=4) :: a(9,3,0,7)
    integer :: i
    integer, allocatable :: r(:,:,:)
    a  = reshape((/ logical(kind=4):: /), shape(a))
    i = 1
    r = count(a, dim=i)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 911
    if (any(ubound(r) /= (/ 3, 0, 7 /))) stop 912
    i = 2
    r = count(a, dim=i)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 913
    if (any(ubound(r) /= (/ 9, 0, 7 /))) stop 914
    i = 3
    r = count(a, dim=i)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 915
    if (any(ubound(r) /= (/ 9, 3, 7 /))) stop 916
    i = 4
    r = count(a, dim=i)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 917
    if (any(ubound(r) /= (/ 9, 3, 0 /))) stop 918
  end subroutine
  subroutine check_findloc_int
    integer :: a(9,3,0,7)
    logical :: m1(9,3,0,7)
    logical(kind=4) :: m4
    integer :: i
    integer, allocatable :: r(:,:,:)
    a  = reshape((/ integer:: /), shape(a))
    m1 = reshape((/ logical:: /), shape(m1))
    m4 = .false.
    i = 1
    r = findloc(a, 10, dim=i)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 1011
    if (any(ubound(r) /= (/ 3, 0, 7 /))) stop 1012
    i = 2
    r = findloc(a, 10, dim=i)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 1013
    if (any(ubound(r) /= (/ 9, 0, 7 /))) stop 1014
    i = 3
    r = findloc(a, 10, dim=i)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 1015
    if (any(ubound(r) /= (/ 9, 3, 7 /))) stop 1016
    i = 4
    r = findloc(a, 10, dim=i)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 1017
    if (any(ubound(r) /= (/ 9, 3, 0 /))) stop 1018
    i = 1
    r = findloc(a, 10, dim=i, mask=m1)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 1021
    if (any(ubound(r) /= (/ 3, 0, 7 /))) stop 1022
    i = 2
    r = findloc(a, 10, dim=i, mask=m1)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 1023
    if (any(ubound(r) /= (/ 9, 0, 7 /))) stop 1024
    i = 3
    r = findloc(a, 10, dim=i, mask=m1)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 1025
    if (any(ubound(r) /= (/ 9, 3, 7 /))) stop 1026
    i = 4
    r = findloc(a, 10, dim=i, mask=m1)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 1027
    if (any(ubound(r) /= (/ 9, 3, 0 /))) stop 1028
    i = 1
    r = findloc(a, 10, dim=i, mask=m4)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 1031
    if (any(ubound(r) /= (/ 3, 0, 7 /))) stop 1032
    i = 2
    r = findloc(a, 10, dim=i, mask=m4)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 1033
    if (any(ubound(r) /= (/ 9, 0, 7 /))) stop 1034
    i = 3
    r = findloc(a, 10, dim=i, mask=m4)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 1035
    if (any(ubound(r) /= (/ 9, 3, 7 /))) stop 1036
    i = 4
    r = findloc(a, 10, dim=i, mask=m4)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 1037
    if (any(ubound(r) /= (/ 9, 3, 0 /))) stop 1038
  end subroutine
  subroutine check_findloc_char
    character :: a(9,3,0,7)
    logical :: m1(9,3,0,7)
    logical(kind=4) :: m4
    integer :: i
    integer, allocatable :: r(:,:,:)
    a  = reshape((/ character:: /), shape(a))
    m1 = reshape((/ logical:: /), shape(m1))
    m4 = .false.
    i = 1
    r = findloc(a, "a", dim=i)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 1111
    if (any(ubound(r) /= (/ 3, 0, 7 /))) stop 1112
    i = 2
    r = findloc(a, "a", dim=i)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 1113
    if (any(ubound(r) /= (/ 9, 0, 7 /))) stop 1114
    i = 3
    r = findloc(a, "a", dim=i)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 1115
    if (any(ubound(r) /= (/ 9, 3, 7 /))) stop 1116
    i = 4
    r = findloc(a, "a", dim=i)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 1117
    if (any(ubound(r) /= (/ 9, 3, 0 /))) stop 1118
    i = 1
    r = findloc(a, "a", dim=i, mask=m1)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 1121
    if (any(ubound(r) /= (/ 3, 0, 7 /))) stop 1122
    i = 2
    r = findloc(a, "a", dim=i, mask=m1)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 1123
    if (any(ubound(r) /= (/ 9, 0, 7 /))) stop 1124
    i = 3
    r = findloc(a, "a", dim=i, mask=m1)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 1125
    if (any(ubound(r) /= (/ 9, 3, 7 /))) stop 1126
    i = 4
    r = findloc(a, "a", dim=i, mask=m1)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 1127
    if (any(ubound(r) /= (/ 9, 3, 0 /))) stop 1128
    i = 1
    r = findloc(a, "a", dim=i, mask=m4)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 1131
    if (any(ubound(r) /= (/ 3, 0, 7 /))) stop 1132
    i = 2
    r = findloc(a, "a", dim=i, mask=m4)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 1133
    if (any(ubound(r) /= (/ 9, 0, 7 /))) stop 1134
    i = 3
    r = findloc(a, "a", dim=i, mask=m4)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 1135
    if (any(ubound(r) /= (/ 9, 3, 7 /))) stop 1136
    i = 4
    r = findloc(a, "a", dim=i, mask=m4)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 1137
    if (any(ubound(r) /= (/ 9, 3, 0 /))) stop 1138
  end subroutine
end program

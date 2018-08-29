! { dg-do run }
! { dg-options "-fbackslash" }

  character(kind=1,len=20) :: s1, t1
  character(kind=4,len=20) :: s4, t4

  call test (4_"ccc  ", 4_"bbb", 4_"ccc", 4_"ddd")
  call test (4_" \xACp  ", 4_" \x900000 ", 4_" \xACp  ", 4_"ddd")
  call test (4_" \xACp  ", 4_" \x900000 ", 4_" \xACp  ", 4_"ddd")

  call test2 (4_" \x900000 ", 4_" \xACp  ", 4_"ddd")

contains

  subroutine test(s4, t4, u4, v4)
    character(kind=4,len=*) :: s4, t4, u4, v4

    if (.not. (s4 >= t4)) STOP 1
    if (.not. (s4 > t4)) STOP 2
    if (.not. (s4 .ge. t4)) STOP 3
    if (.not. (s4 .gt. t4)) STOP 4
    if (      (s4 == t4)) STOP 5
    if (.not. (s4 /= t4)) STOP 6
    if (      (s4 .eq. t4)) STOP 7
    if (.not. (s4 .ne. t4)) STOP 8
    if (      (s4 <= t4)) STOP 9
    if (      (s4 < t4)) STOP 10
    if (      (s4 .le. t4)) STOP 11
    if (      (s4 .lt. t4)) STOP 12

    if (.not. (s4 >= u4)) STOP 13
    if (      (s4 > u4)) STOP 14
    if (.not. (s4 .ge. u4)) STOP 15
    if (      (s4 .gt. u4)) STOP 16
    if (.not. (s4 == u4)) STOP 17
    if (      (s4 /= u4)) STOP 18
    if (.not. (s4 .eq. u4)) STOP 19
    if (      (s4 .ne. u4)) STOP 20
    if (.not. (s4 <= u4)) STOP 21
    if (      (s4 < u4)) STOP 22
    if (.not. (s4 .le. u4)) STOP 23
    if (      (s4 .lt. u4)) STOP 24

    if (      (s4 >= v4)) STOP 25
    if (      (s4 > v4)) STOP 26
    if (      (s4 .ge. v4)) STOP 27
    if (      (s4 .gt. v4)) STOP 28
    if (      (s4 == v4)) STOP 29
    if (.not. (s4 /= v4)) STOP 30
    if (      (s4 .eq. v4)) STOP 31
    if (.not. (s4 .ne. v4)) STOP 32
    if (.not. (s4 <= v4)) STOP 33
    if (.not. (s4 < v4)) STOP 34
    if (.not. (s4 .le. v4)) STOP 35
    if (.not. (s4 .lt. v4)) STOP 36

  end subroutine test

  subroutine test2(t4, u4, v4)
    character(kind=4,len=*) :: t4, u4, v4

    if (.not. (4_" \xACp  " >= t4)) STOP 37
    if (.not. (4_" \xACp  " > t4)) STOP 38
    if (.not. (4_" \xACp  " .ge. t4)) STOP 39
    if (.not. (4_" \xACp  " .gt. t4)) STOP 40
    if (      (4_" \xACp  " == t4)) STOP 41
    if (.not. (4_" \xACp  " /= t4)) STOP 42
    if (      (4_" \xACp  " .eq. t4)) STOP 43
    if (.not. (4_" \xACp  " .ne. t4)) STOP 44
    if (      (4_" \xACp  " <= t4)) STOP 45
    if (      (4_" \xACp  " < t4)) STOP 46
    if (      (4_" \xACp  " .le. t4)) STOP 47
    if (      (4_" \xACp  " .lt. t4)) STOP 48

    if (.not. (4_" \xACp  " >= u4)) STOP 49
    if (      (4_" \xACp  " > u4)) STOP 50
    if (.not. (4_" \xACp  " .ge. u4)) STOP 51
    if (      (4_" \xACp  " .gt. u4)) STOP 52
    if (.not. (4_" \xACp  " == u4)) STOP 53
    if (      (4_" \xACp  " /= u4)) STOP 54
    if (.not. (4_" \xACp  " .eq. u4)) STOP 55
    if (      (4_" \xACp  " .ne. u4)) STOP 56
    if (.not. (4_" \xACp  " <= u4)) STOP 57
    if (      (4_" \xACp  " < u4)) STOP 58
    if (.not. (4_" \xACp  " .le. u4)) STOP 59
    if (      (4_" \xACp  " .lt. u4)) STOP 60

    if (      (4_" \xACp  " >= v4)) STOP 61
    if (      (4_" \xACp  " > v4)) STOP 62
    if (      (4_" \xACp  " .ge. v4)) STOP 63
    if (      (4_" \xACp  " .gt. v4)) STOP 64
    if (      (4_" \xACp  " == v4)) STOP 65
    if (.not. (4_" \xACp  " /= v4)) STOP 66
    if (      (4_" \xACp  " .eq. v4)) STOP 67
    if (.not. (4_" \xACp  " .ne. v4)) STOP 68
    if (.not. (4_" \xACp  " <= v4)) STOP 69
    if (.not. (4_" \xACp  " < v4)) STOP 70
    if (.not. (4_" \xACp  " .le. v4)) STOP 71
    if (.not. (4_" \xACp  " .lt. v4)) STOP 72

  end subroutine test2

  subroutine test3(t4, u4, v4)
    character(kind=4,len=*) :: t4, u4, v4

    if (.not. (4_" \xACp  " >= 4_" \x900000 ")) STOP 73
    if (.not. (4_" \xACp  " > 4_" \x900000 ")) STOP 74
    if (.not. (4_" \xACp  " .ge. 4_" \x900000 ")) STOP 75
    if (.not. (4_" \xACp  " .gt. 4_" \x900000 ")) STOP 76
    if (      (4_" \xACp  " == 4_" \x900000 ")) STOP 77
    if (.not. (4_" \xACp  " /= 4_" \x900000 ")) STOP 78
    if (      (4_" \xACp  " .eq. 4_" \x900000 ")) STOP 79
    if (.not. (4_" \xACp  " .ne. 4_" \x900000 ")) STOP 80
    if (      (4_" \xACp  " <= 4_" \x900000 ")) STOP 81
    if (      (4_" \xACp  " < 4_" \x900000 ")) STOP 82
    if (      (4_" \xACp  " .le. 4_" \x900000 ")) STOP 83
    if (      (4_" \xACp  " .lt. 4_" \x900000 ")) STOP 84

    if (.not. (4_" \xACp  " >= 4_" \xACp  ")) STOP 85
    if (      (4_" \xACp  " > 4_" \xACp  ")) STOP 86
    if (.not. (4_" \xACp  " .ge. 4_" \xACp  ")) STOP 87
    if (      (4_" \xACp  " .gt. 4_" \xACp  ")) STOP 88
    if (.not. (4_" \xACp  " == 4_" \xACp  ")) STOP 89
    if (      (4_" \xACp  " /= 4_" \xACp  ")) STOP 90
    if (.not. (4_" \xACp  " .eq. 4_" \xACp  ")) STOP 91
    if (      (4_" \xACp  " .ne. 4_" \xACp  ")) STOP 92
    if (.not. (4_" \xACp  " <= 4_" \xACp  ")) STOP 93
    if (      (4_" \xACp  " < 4_" \xACp  ")) STOP 94
    if (.not. (4_" \xACp  " .le. 4_" \xACp  ")) STOP 95
    if (      (4_" \xACp  " .lt. 4_" \xACp  ")) STOP 96

    if (      (4_" \xACp  " >= 4_"ddd")) STOP 97
    if (      (4_" \xACp  " > 4_"ddd")) STOP 98
    if (      (4_" \xACp  " .ge. 4_"ddd")) STOP 99
    if (      (4_" \xACp  " .gt. 4_"ddd")) STOP 100
    if (      (4_" \xACp  " == 4_"ddd")) STOP 101
    if (.not. (4_" \xACp  " /= 4_"ddd")) STOP 102
    if (      (4_" \xACp  " .eq. 4_"ddd")) STOP 103
    if (.not. (4_" \xACp  " .ne. 4_"ddd")) STOP 104
    if (.not. (4_" \xACp  " <= 4_"ddd")) STOP 105
    if (.not. (4_" \xACp  " < 4_"ddd")) STOP 106
    if (.not. (4_" \xACp  " .le. 4_"ddd")) STOP 107
    if (.not. (4_" \xACp  " .lt. 4_"ddd")) STOP 108

  end subroutine test3

end

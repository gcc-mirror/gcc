! { dg-do run }
!
! Check fix for correctly deep copying allocatable components.
! PR fortran/59678
! Contributed by Andre Vehreschild  <vehre@gmx.de>
!
program alloc_comp_copy_test

  type InnerT
    integer :: ii
    integer, allocatable :: ai
    integer, allocatable :: v(:)
  end type InnerT

  type T
    integer :: i
    integer, allocatable :: a_i
    type(InnerT), allocatable :: it
    type(InnerT), allocatable :: vec(:)
  end type T

  type(T) :: o1, o2
  class(T), allocatable :: o3, o4
  o1%i = 42

  call copyO(o1, o2)
  if (o2%i /= 42) STOP 1
  if (allocated(o2%a_i)) STOP 2
  if (allocated(o2%it)) STOP 3
  if (allocated(o2%vec)) STOP 4

  allocate (o1%a_i, source=2)
  call copyO(o1, o2)
  if (o2%i /= 42) STOP 5
  if (.not. allocated(o2%a_i)) STOP 6
  if (o2%a_i /= 2) STOP 7
  if (allocated(o2%it)) STOP 8
  if (allocated(o2%vec)) STOP 9

  allocate (o1%it)
  o1%it%ii = 3
  call copyO(o1, o2)
  if (o2%i /= 42) STOP 10
  if (.not. allocated(o2%a_i)) STOP 11
  if (o2%a_i /= 2) STOP 12
  if (.not. allocated(o2%it)) STOP 13
  if (o2%it%ii /= 3) STOP 14
  if (allocated(o2%it%ai)) STOP 15
  if (allocated(o2%it%v)) STOP 16
  if (allocated(o2%vec)) STOP 17

  allocate (o1%it%ai)
  o1%it%ai = 4
  call copyO(o1, o2)
  if (o2%i /= 42) STOP 18
  if (.not. allocated(o2%a_i)) STOP 19
  if (o2%a_i /= 2) STOP 20
  if (.not. allocated(o2%it)) STOP 21
  if (o2%it%ii /= 3) STOP 22
  if (.not. allocated(o2%it%ai)) STOP 23
  if (o2%it%ai /= 4) STOP 24
  if (allocated(o2%it%v)) STOP 25
  if (allocated(o2%vec)) STOP 26

  allocate (o1%it%v(3), source= 5)
  call copyO(o1, o2)
  if (o2%i /= 42) STOP 27
  if (.not. allocated(o2%a_i)) STOP 28
  if (o2%a_i /= 2) STOP 29
  if (.not. allocated(o2%it)) STOP 30
  if (o2%it%ii /= 3) STOP 31
  if (.not. allocated(o2%it%ai)) STOP 32
  if (o2%it%ai /= 4) STOP 33
  if (.not. allocated(o2%it%v)) STOP 34
  if (any (o2%it%v /= 5) .or. size (o2%it%v) /= 3) STOP 35
  if (allocated(o2%vec)) STOP 36

  allocate (o1%vec(2))
  o1%vec(:)%ii = 6
  call copyO(o1, o2)
  if (o2%i /= 42) STOP 37
  if (.not. allocated(o2%a_i)) STOP 38
  if (o2%a_i /= 2) STOP 39
  if (.not. allocated(o2%it)) STOP 40
  if (o2%it%ii /= 3) STOP 41
  if (.not. allocated(o2%it%ai)) STOP 42
  if (o2%it%ai /= 4) STOP 43
  if (.not. allocated(o2%it%v)) STOP 44
  if (size (o2%it%v) /= 3) STOP 45
  if (any (o2%it%v /= 5)) STOP 46
  if (.not. allocated(o2%vec)) STOP 47
  if (size(o2%vec) /= 2) STOP 48
  if (any(o2%vec(:)%ii /= 6)) STOP 49
  if (allocated(o2%vec(1)%ai) .or. allocated(o2%vec(2)%ai)) STOP 50
  if (allocated(o2%vec(1)%v) .or. allocated(o2%vec(2)%v)) STOP 51

  allocate (o1%vec(2)%ai)
  o1%vec(2)%ai = 7
  call copyO(o1, o2)
  if (o2%i /= 42) STOP 52
  if (.not. allocated(o2%a_i)) STOP 53
  if (o2%a_i /= 2) STOP 54
  if (.not. allocated(o2%it)) STOP 55
  if (o2%it%ii /= 3) STOP 56
  if (.not. allocated(o2%it%ai)) STOP 57
  if (o2%it%ai /= 4) STOP 58
  if (.not. allocated(o2%it%v)) STOP 59
  if (size (o2%it%v) /= 3) STOP 60
  if (any (o2%it%v /= 5)) STOP 61
  if (.not. allocated(o2%vec)) STOP 62
  if (size(o2%vec) /= 2) STOP 63
  if (any(o2%vec(:)%ii /= 6)) STOP 64
  if (allocated(o2%vec(1)%ai)) STOP 65
  if (.not. allocated(o2%vec(2)%ai)) STOP 66
  if (o2%vec(2)%ai /= 7) STOP 67
  if (allocated(o2%vec(1)%v) .or. allocated(o2%vec(2)%v)) STOP 68

  allocate (o1%vec(1)%v(3))
  o1%vec(1)%v = [8, 9, 10]
  call copyO(o1, o2)
  if (o2%i /= 42) STOP 69
  if (.not. allocated(o2%a_i)) STOP 70
  if (o2%a_i /= 2) STOP 71
  if (.not. allocated(o2%it)) STOP 72
  if (o2%it%ii /= 3) STOP 73
  if (.not. allocated(o2%it%ai)) STOP 74
  if (o2%it%ai /= 4) STOP 75
  if (.not. allocated(o2%it%v)) STOP 76
  if (size (o2%it%v) /= 3) STOP 77
  if (any (o2%it%v /= 5)) STOP 78
  if (.not. allocated(o2%vec)) STOP 79
  if (size(o2%vec) /= 2) STOP 80
  if (any(o2%vec(:)%ii /= 6)) STOP 81
  if (allocated(o2%vec(1)%ai)) STOP 82
  if (.not. allocated(o2%vec(2)%ai)) STOP 83
  if (o2%vec(2)%ai /= 7) STOP 84
  if (.not. allocated(o2%vec(1)%v)) STOP 85
  if (any (o2%vec(1)%v /= [8,9,10])) STOP 86
  if (allocated(o2%vec(2)%v)) STOP 87

  ! Now all the above for class objects.
  allocate (o3, o4)
  o3%i = 42

  call copyO(o3, o4)
  if (o4%i /= 42) STOP 88
  if (allocated(o4%a_i)) STOP 89
  if (allocated(o4%it)) STOP 90
  if (allocated(o4%vec)) STOP 91

  allocate (o3%a_i, source=2)
  call copyO(o3, o4)
  if (o4%i /= 42) STOP 92
  if (.not. allocated(o4%a_i)) STOP 93
  if (o4%a_i /= 2) STOP 94
  if (allocated(o4%it)) STOP 95
  if (allocated(o4%vec)) STOP 96

  allocate (o3%it)
  o3%it%ii = 3
  call copyO(o3, o4)
  if (o4%i /= 42) STOP 97
  if (.not. allocated(o4%a_i)) STOP 98
  if (o4%a_i /= 2) STOP 99
  if (.not. allocated(o4%it)) STOP 100
  if (o4%it%ii /= 3) STOP 101
  if (allocated(o4%it%ai)) STOP 102
  if (allocated(o4%it%v)) STOP 103
  if (allocated(o4%vec)) STOP 104

  allocate (o3%it%ai)
  o3%it%ai = 4
  call copyO(o3, o4)
  if (o4%i /= 42) STOP 105
  if (.not. allocated(o4%a_i)) STOP 106
  if (o4%a_i /= 2) STOP 107
  if (.not. allocated(o4%it)) STOP 108
  if (o4%it%ii /= 3) STOP 109
  if (.not. allocated(o4%it%ai)) STOP 110
  if (o4%it%ai /= 4) STOP 111
  if (allocated(o4%it%v)) STOP 112
  if (allocated(o4%vec)) STOP 113

  allocate (o3%it%v(3), source= 5)
  call copyO(o3, o4)
  if (o4%i /= 42) STOP 114
  if (.not. allocated(o4%a_i)) STOP 115
  if (o4%a_i /= 2) STOP 116
  if (.not. allocated(o4%it)) STOP 117
  if (o4%it%ii /= 3) STOP 118
  if (.not. allocated(o4%it%ai)) STOP 119
  if (o4%it%ai /= 4) STOP 120
  if (.not. allocated(o4%it%v)) STOP 121
  if (any (o4%it%v /= 5) .or. size (o4%it%v) /= 3) STOP 122
  if (allocated(o4%vec)) STOP 123

  allocate (o3%vec(2))
  o3%vec(:)%ii = 6
  call copyO(o3, o4)
  if (o4%i /= 42) STOP 124
  if (.not. allocated(o4%a_i)) STOP 125
  if (o4%a_i /= 2) STOP 126
  if (.not. allocated(o4%it)) STOP 127
  if (o4%it%ii /= 3) STOP 128
  if (.not. allocated(o4%it%ai)) STOP 129
  if (o4%it%ai /= 4) STOP 130
  if (.not. allocated(o4%it%v)) STOP 131
  if (size (o4%it%v) /= 3) STOP 132
  if (any (o4%it%v /= 5)) STOP 133
  if (.not. allocated(o4%vec)) STOP 134
  if (size(o4%vec) /= 2) STOP 135
  if (any(o4%vec(:)%ii /= 6)) STOP 136
  if (allocated(o4%vec(1)%ai) .or. allocated(o4%vec(2)%ai)) STOP 137
  if (allocated(o4%vec(1)%v) .or. allocated(o4%vec(2)%v)) STOP 138

  allocate (o3%vec(2)%ai)
  o3%vec(2)%ai = 7
  call copyO(o3, o4)
  if (o4%i /= 42) STOP 139
  if (.not. allocated(o4%a_i)) STOP 140
  if (o4%a_i /= 2) STOP 141
  if (.not. allocated(o4%it)) STOP 142
  if (o4%it%ii /= 3) STOP 143
  if (.not. allocated(o4%it%ai)) STOP 144
  if (o4%it%ai /= 4) STOP 145
  if (.not. allocated(o4%it%v)) STOP 146
  if (size (o4%it%v) /= 3) STOP 147
  if (any (o4%it%v /= 5)) STOP 148
  if (.not. allocated(o4%vec)) STOP 149
  if (size(o4%vec) /= 2) STOP 150
  if (any(o4%vec(:)%ii /= 6)) STOP 151
  if (allocated(o4%vec(1)%ai)) STOP 152
  if (.not. allocated(o4%vec(2)%ai)) STOP 153
  if (o4%vec(2)%ai /= 7) STOP 154
  if (allocated(o4%vec(1)%v) .or. allocated(o4%vec(2)%v)) STOP 155

  allocate (o3%vec(1)%v(3))
  o3%vec(1)%v = [8, 9, 10]
  call copyO(o3, o4)
  if (o4%i /= 42) STOP 156
  if (.not. allocated(o4%a_i)) STOP 157
  if (o4%a_i /= 2) STOP 158
  if (.not. allocated(o4%it)) STOP 159
  if (o4%it%ii /= 3) STOP 160
  if (.not. allocated(o4%it%ai)) STOP 161
  if (o4%it%ai /= 4) STOP 162
  if (.not. allocated(o4%it%v)) STOP 163
  if (size (o4%it%v) /= 3) STOP 164
  if (any (o4%it%v /= 5)) STOP 165
  if (.not. allocated(o4%vec)) STOP 166
  if (size(o4%vec) /= 2) STOP 167
  if (any(o4%vec(:)%ii /= 6)) STOP 168
  if (allocated(o4%vec(1)%ai)) STOP 169
  if (.not. allocated(o4%vec(2)%ai)) STOP 170
  if (o4%vec(2)%ai /= 7) STOP 171
  if (.not. allocated(o4%vec(1)%v)) STOP 172
  if (any (o4%vec(1)%v /= [8,9,10])) STOP 173
  if (allocated(o4%vec(2)%v)) STOP 174

contains

  subroutine copyO(src, dst)
    type(T), intent(in) :: src
    type(T), intent(out) :: dst

    dst = src
  end subroutine copyO

end program alloc_comp_copy_test


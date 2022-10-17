implicit none
type t2
  integer :: x
end type t2

type, extends(t2) :: t2e
  integer :: y
end type t2e

type t
  class(*), allocatable :: au, au2(:,:)
  class(t2), allocatable :: at, at2(:,:)
end type t

type(t), target :: var, var0, var2(4), var2a(4)
class(*), allocatable :: au, au2(:,:)
class(t2), allocatable :: at, at2(:,:)


if (same_type_as (var%au, var%at)) error stop 1
if (same_type_as (var%au2, var%at)) error stop 2
if (same_type_as (var%au, var%at)) error stop 3
! Note: class(*) has no declared type, hence .false.
if (same_type_as (var%au, var0%au)) error stop 4
if (same_type_as (var%au2, var0%au2)) error stop 5
if (same_type_as (var%au, var0%au2)) error stop 6
call c1(var%au, var%au, var%au2)

if (.not.same_type_as (var%at, var%at)) error stop 7
if (.not.same_type_as (var%at2, var%at)) error stop 8
if (.not.same_type_as (var%at, var%at2)) error stop 9
if (.not.extends_type_of (var%at, var%at)) error stop 10
if (.not.extends_type_of (var%at2, var%at)) error stop 11
if (.not.extends_type_of (var%at, var%at2)) error stop 12
if (same_type_as (var%at, var0%au)) error stop 13
if (same_type_as (var%at2, var0%au2)) error stop 14
if (same_type_as (var%at, var0%au2)) error stop 15
call c2(var%at, var%at, var%at2)

if (same_type_as (au, var%at)) error stop 16
if (same_type_as (au2, var%at)) error stop 17
if (same_type_as (au, var%at)) error stop 18
! Note: class(*) has no declared type, hence .false.
if (same_type_as (au, var0%au)) error stop 19
if (same_type_as (au2, var0%au2)) error stop 20
if (same_type_as (au, var0%au2)) error stop 21
call c1(au, var%au, var%au2)

if (.not.same_type_as (at, var%at)) error stop 22
if (.not.same_type_as (at2, var%at)) error stop 23
if (.not.same_type_as (at, var%at2)) error stop 24
if (.not.extends_type_of (at, var%at)) error stop 25
if (.not.extends_type_of (at2, var%at)) error stop 26
if (.not.extends_type_of (at, var%at2)) error stop 27
if (same_type_as (at, var0%au)) error stop 28
if (same_type_as (at2, var0%au2)) error stop 29
if (same_type_as (at, var0%au2)) error stop 30
call c2(var%at, var%at, var%at2)

if (same_type_as (var%au, at)) error stop 31
if (same_type_as (var%au2, at)) error stop 32
if (same_type_as (var%au, at)) error stop 33
! Note: class(*) has no declared type, hence .false.
if (same_type_as (var%au, au)) error stop 34
if (same_type_as (var%au2, au2)) error stop 35
if (same_type_as (var%au, au2)) error stop 36
call c1(var%au, var%au, au2)

if (.not.same_type_as (var%at, at)) error stop 37
if (.not.same_type_as (var%at2, at)) error stop 38
if (.not.same_type_as (var%at, at2)) error stop 39
if (.not.extends_type_of (var%at, at)) error stop 40
if (.not.extends_type_of (var%at2, at)) error stop 41
if (.not.extends_type_of (var%at, at2)) error stop 42
if (same_type_as (var%at, au)) error stop 43
if (same_type_as (var%at2, au2)) error stop 44
if (same_type_as (var%at, au2)) error stop 45
call c2(var%at, var%at, at2)

allocate(t2e :: var0%at, var0%at2(4,4))
allocate(t2 :: var0%au, var0%au2(4,4))

if (.not.same_type_as (var0%au, var%at)) error stop 46
if (.not.same_type_as (var0%au2, var%at)) error stop 47
if (.not.same_type_as (var0%au, var%at)) error stop 48
if (.not.same_type_as (var0%au, var0%au2)) error stop 49
if (.not.same_type_as (var0%au2, var0%au2)) error stop 50
if (.not.same_type_as (var0%au, var0%au2)) error stop 51
if (.not.extends_type_of (var0%au, var%at)) error stop 52
if (.not.extends_type_of (var0%au2, var%at)) error stop 53
if (.not.extends_type_of (var0%au, var%at)) error stop 54
if (.not.extends_type_of (var0%au, var0%au2)) error stop 55
if (.not.extends_type_of (var0%au2, var0%au2)) error stop 56
if (.not.extends_type_of (var0%au, var0%au2)) error stop 57

if (.not.same_type_as (var0%au, at)) error stop 58
if (.not.same_type_as (var0%au2, at)) error stop 59
if (.not.same_type_as (var0%au, at2)) error stop 60
if (.not.extends_type_of (var0%au, at)) error stop 61
if (.not.extends_type_of (var0%au2, at)) error stop 62
if (.not.extends_type_of (var0%au, at2)) error stop 63

if (same_type_as (var0%at, var%at)) error stop 64
if (same_type_as (var0%at2, var%at)) error stop 65
if (same_type_as (var0%at, var%at)) error stop 66
if (same_type_as (var0%at, var0%au2)) error stop 67
if (same_type_as (var0%at2, var0%au2)) error stop 68
if (same_type_as (var0%at, var0%au2)) error stop 69
if (.not.extends_type_of (var0%at, var%at)) error stop 70
if (.not.extends_type_of (var0%at2, var%at)) error stop 71
if (.not.extends_type_of (var0%at, var%at)) error stop 72
if (.not.extends_type_of (var0%at, var0%au2)) error stop 73
if (.not.extends_type_of (var0%at2, var0%au2)) error stop 74
if (.not.extends_type_of (var0%at, var0%au2)) error stop 75

if (same_type_as (var0%at, at)) error stop 76
if (same_type_as (var0%at2, at)) error stop 77
if (same_type_as (var0%at, at2)) error stop 78
if (.not.extends_type_of (var0%at, at)) error stop 79
if (.not.extends_type_of (var0%at2, at)) error stop 80
if (.not.extends_type_of (var0%at, at2)) error stop 81

call c3(var0%au, var0%au2, var0%at, var0%at2)
call c4(var0%au, var0%au2, var0%at, var0%at2)

contains
  subroutine c1(x, y, z)
    class(*) :: x, y(..), z(..)
    if (same_type_as (x, var0%at)) error stop 82
    if (same_type_as (y, var0%at)) error stop 83
    if (same_type_as (z, var0%at)) error stop 84
    if (same_type_as (x, var%au)) error stop 85
    if (same_type_as (y, var%au2)) error stop 86
    if (same_type_as (z, var%au2)) error stop 87

    if (same_type_as (x, at)) error stop 88
    if (same_type_as (y, at)) error stop 89
    if (same_type_as (z, at)) error stop 90
    if (same_type_as (x, au)) error stop 91
    if (same_type_as (y, au2)) error stop 92
    if (same_type_as (z, au2)) error stop 93
  end

  subroutine c2(x, y, z)
    class(*) :: x, y(..), z(..)
    if (.not.same_type_as (x, var0%at)) error stop 94
    if (.not.same_type_as (y, var0%at)) error stop 95
    if (.not.same_type_as (z, var0%at)) error stop 96
    if (.not.extends_type_of (x, var0%at)) error stop 97
    if (.not.extends_type_of (y, var0%at)) error stop 98
    if (.not.extends_type_of (z, var0%at)) error stop 99
    if (same_type_as (x, var%au)) error stop 100
    if (same_type_as (y, var%au2)) error stop 101
    if (same_type_as (z, var%au2)) error stop 102

    if (.not.same_type_as (x, at)) error stop 103
    if (.not.same_type_as (y, at)) error stop 104
    if (.not.same_type_as (z, at)) error stop 105
    if (.not.extends_type_of (x, at)) error stop 106
    if (.not.extends_type_of (y, at)) error stop 107
    if (.not.extends_type_of (z, at)) error stop 108
    if (same_type_as (x, au)) error stop 109
    if (same_type_as (y, au2)) error stop 110
    if (same_type_as (z, au2)) error stop 111
  end

  subroutine c3(mau, mau2, mat, mat2)
    class(*) :: mau, mau2(:,:), mat, mat2(:,:)

    if (.not.same_type_as (mau, var%at)) error stop 112
    if (.not.same_type_as (mau2, var%at)) error stop 113
    if (.not.same_type_as (mau, var%at)) error stop 114
    if (.not.same_type_as (mau, var0%au2)) error stop 115
    if (.not.same_type_as (mau2, var0%au2)) error stop 116
    if (.not.same_type_as (mau, var0%au2)) error stop 117
    if (.not.extends_type_of (mau, var%at)) error stop 118
    if (.not.extends_type_of (mau2, var%at)) error stop 119
    if (.not.extends_type_of (mau, var%at)) error stop 120
    if (.not.extends_type_of (mau, var0%au2)) error stop 121
    if (.not.extends_type_of (mau2, var0%au2)) error stop 122
    if (.not.extends_type_of (mau, var0%au2)) error stop 123

    if (.not.same_type_as (mau, at)) error stop 124
    if (.not.same_type_as (mau2, at)) error stop 125
    if (.not.same_type_as (mau, at2)) error stop 126
    if (.not.extends_type_of (mau, at)) error stop 127
    if (.not.extends_type_of (mau2, at)) error stop 128
    if (.not.extends_type_of (mau, at2)) error stop 129

    if (same_type_as (mat, var%at)) error stop 130
    if (same_type_as (mat2, var%at)) error stop 131
    if (same_type_as (mat, var%at)) error stop 132
    if (same_type_as (mat, var0%au2)) error stop 133
    if (same_type_as (mat2, var0%au2)) error stop 134
    if (same_type_as (mat, var0%au2)) error stop 135
    if (.not.extends_type_of (mat, var%at)) error stop 136
    if (.not.extends_type_of (mat2, var%at)) error stop 137
    if (.not.extends_type_of (mat, var%at)) error stop 138
    if (.not.extends_type_of (mat, var0%au2)) error stop 139
    if (.not.extends_type_of (mat2, var0%au2)) error stop 140
    if (.not.extends_type_of (mat, var0%au2)) error stop 141

    if (same_type_as (mat, at)) error stop 142
    if (same_type_as (mat2, at)) error stop 143
    if (same_type_as (mat, at2)) error stop 144
    if (.not.extends_type_of (mat, at)) error stop 145
    if (.not.extends_type_of (mat2, at)) error stop 147
    if (.not.extends_type_of (mat, at2)) error stop 148
  end

  subroutine c4(mau, mau2, mat, mat2)
    class(*) :: mau(..), mau2(..), mat(..), mat2(..)

    if (.not.same_type_as (mau, var%at)) error stop 149
    if (.not.same_type_as (mau2, var%at)) error stop 150
    if (.not.same_type_as (mau, var%at)) error stop 151
    if (.not.same_type_as (mau, var0%au2)) error stop 152
    if (.not.same_type_as (mau2, var0%au2)) error stop 153
    if (.not.same_type_as (mau, var0%au2)) error stop 154
    if (.not.extends_type_of (mau, var%at)) error stop 155
    if (.not.extends_type_of (mau2, var%at)) error stop 156
    if (.not.extends_type_of (mau, var%at)) error stop 157
    if (.not.extends_type_of (mau, var0%au2)) error stop 158
    if (.not.extends_type_of (mau2, var0%au2)) error stop 159
    if (.not.extends_type_of (mau, var0%au2)) error stop 160

    if (.not.same_type_as (mau, at)) error stop 161
    if (.not.same_type_as (mau2, at)) error stop 162
    if (.not.same_type_as (mau, at2)) error stop 163
    if (.not.extends_type_of (mau, at)) error stop 164
    if (.not.extends_type_of (mau2, at)) error stop 165
    if (.not.extends_type_of (mau, at2)) error stop 166

    if (same_type_as (mat, var%at)) error stop 167
    if (same_type_as (mat2, var%at)) error stop 168
    if (same_type_as (mat, var%at)) error stop 169
    if (same_type_as (mat, var0%au2)) error stop 170
    if (same_type_as (mat2, var0%au2)) error stop 171
    if (same_type_as (mat, var0%au2)) error stop 172
    if (.not.extends_type_of (mat, var%at)) error stop 173
    if (.not.extends_type_of (mat2, var%at)) error stop 174
    if (.not.extends_type_of (mat, var%at)) error stop 175
    if (.not.extends_type_of (mat, var0%au2)) error stop 176
    if (.not.extends_type_of (mat2, var0%au2)) error stop 178
    if (.not.extends_type_of (mat, var0%au2)) error stop 179

    if (same_type_as (mat, at)) error stop 180
    if (same_type_as (mat2, at)) error stop 181
    if (same_type_as (mat, at2)) error stop 182
    if (.not.extends_type_of (mat, at)) error stop 183
    if (.not.extends_type_of (mat2, at)) error stop 184
    if (.not.extends_type_of (mat, at2)) error stop 185
  end
end

! { dg-do compile }
!
! PR 44929: [OOP] Parsing error of derived type name starting with 'REAL'
!
! Contributed by Satish.BD <bdsatish@gmail.com>

 type :: real_type
 end type
 class(real_type), allocatable :: obj
 real(8), allocatable :: r8

 allocate(real_type :: obj)

 allocate( real(kind=8) :: r8)
 allocate(real(8)  :: r8 )

end 

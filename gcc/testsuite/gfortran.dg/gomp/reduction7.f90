implicit none
integer :: a, b, i
a = 0

!$omp simd reduction(task,+:a)  ! { dg-error "invalid 'task' reduction modifier on construct other than 'parallel', 'do', 'sections' or 'scope'" }
do i=1,10
  a = a + 1
end do
end

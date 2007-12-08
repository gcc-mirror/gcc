# 1 "include_2.F90"
# 1 "/tmp/"
# 1 "<built-in>"
# 1 "<command line>"
# 1 "include_2.F90"
#define S1 1
#define B a
# 1 "include_2.inc" 1
subroutine a
#undef S2
#define S2 1
integer :: i
end subroutine a
# 4 "include_2.F90" 2
#undef B
#define B b
# 1 "include_2.inc" 1
subroutine b
#undef S2
#define S2 1
integer :: i
end subroutine b
# 6 "include_2.F90" 2
! PR debug/33739
! { dg-do link }
! { dg-options "-fpreprocessed -g3" }
    call a
    call b
end

! { dg-do compile }
! PR 29393: Ranks of PARAMETER-lhs in initializations
    integer, parameter ::   A(-3:7,2)=0 
    integer, parameter, dimension(3) :: V = (/ 2, 4, 6 /)
    integer, parameter, dimension(3) :: B = A(V,1) 
    integer, parameter, dimension(3) :: C = A(0:2,1) 
end

! { dg-do run }
program p
 implicit none

 real   , parameter :: arr(3) = [ real    :: 2, 2.5, (1.5, 2.5) ]
 real   , parameter :: ari(3) = [ integer :: 2, 2.5, (1.5, 2.5) ]
 real   , parameter :: arc(3) = [ complex :: 2, 2.5, (1.5, 2.5) ]
 integer, parameter :: air(3) = [ real    :: 2, 2.5, (1.5, 2.5) ]
 integer, parameter :: aii(3) = [ integer :: 2, 2.5, (1.5, 2.5) ]
 integer, parameter :: aic(3) = [ complex :: 2, 2.5, (1.5, 2.5) ]
 complex, parameter :: acr(3) = [ real    :: 2, 2.5, (1.5, 2.5) ]
 complex, parameter :: aci(3) = [ integer :: 2, 2.5, (1.5, 2.5) ]
 complex, parameter :: acc(3) = [ complex :: 2, 2.5, (1.5, 2.5) ]

 real   , parameter :: mrr(3) =  4.5       * [ real    :: 2, 2.5, (3.5, 4.0) ]
 real   , parameter :: mri(3) =  4.5       * [ integer :: 2, 2.5, (3.5, 4.0) ]
 real   , parameter :: mrc(3) =  4.5       * [ complex :: 2, 2.5, (3.5, 4.0) ]
 integer, parameter :: mir(3) =  4         * [ real    :: 2, 2.5, (3.5, 4.0) ]
 integer, parameter :: mii(3) =  4         * [ integer :: 2, 2.5, (3.5, 4.0) ]
 integer, parameter :: mic(3) =  4         * [ complex :: 2, 2.5, (3.5, 4.0) ]
 complex, parameter :: mcr(3) = (4.5, 5.5) * [ real    :: 2, 2.5, (3.5, 4.0) ]
 complex, parameter :: mci(3) = (4.5, 5.5) * [ integer :: 2, 2.5, (3.5, 4.0) ]
 complex, parameter :: mcc(3) = (4.5, 5.5) * [ complex :: 2, 2.5, (3.5, 4.0) ]

 if (any(arr /= [2.00, 2.50, 1.50])) STOP 1
 if (any(ari /= [2.00, 2.00, 1.00])) STOP 2
 if (any(arc /= [2.00, 2.50, 1.50])) STOP 3

 if (any(air /= [2, 2, 1])) STOP 4
 if (any(aii /= [2, 2, 1])) STOP 5
 if (any(aic /= [2, 2, 1])) STOP 6

 if (any(acr /= [(2.00, 0.00), (2.50, 0.00), (1.50, 0.00)])) STOP 7
 if (any(aci /= [(2.00, 0.00), (2.00, 0.00), (1.00, 0.00)])) STOP 8
 if (any(acc /= [(2.00, 0.00), (2.50, 0.00), (1.50, 2.50)])) STOP 9

 if (any(mrr /= [9.00, 11.25, 15.75])) STOP 10
 if (any(mri /= [9.00,  9.00, 13.50])) STOP 11
 if (any(mrc /= [9.00, 11.25, 15.75])) STOP 12

 if (any(mir /= [8, 10, 14])) STOP 13
 if (any(mii /= [8,  8, 12])) STOP 14
 if (any(mic /= [8, 10, 14])) STOP 15

 if (any(mcr /= [(9.00, 11.00), (11.25, 13.75), (15.75, 19.25)])) STOP 16
 if (any(mci /= [(9.00, 11.00), ( 9.00, 11.00), (13.50, 16.50)])) STOP 17
 if (any(mcc /= [(9.00, 11.00), (11.25, 13.75), (-6.25, 37.25)])) STOP 18

end program p

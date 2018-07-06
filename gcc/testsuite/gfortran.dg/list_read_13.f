c { dg-do run }
c PR61049, reduced test case by  Dominique d'Humieres
      character(len=30) :: buff = ", (2.0, 3.0),,6.0D0, 2*,"
      DOUBLE PRECISION AVD, BVD, CVD, DVCORR 
      COMPLEX AVC, BVC, CVC, ZVCORR
      
      read(buff, *, err=10)  AVD, AVC, BVC, BVD, CVC, CVD
      goto 20
 10   STOP 1
 20   continue       
      end
    
  

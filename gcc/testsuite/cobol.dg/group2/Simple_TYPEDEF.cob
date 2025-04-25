       *> { dg-do run }
       *> { dg-options "-dialect mf" }

        identification division.
        program-id. wrapper.
        data division.
        working-storage section.
        77  UNS-CHAR      PIC  9(02)  COMP-5 IS TYPEDEF.
        01  Z-H3          PIC  X(017) .
        01  I-H3A         USAGE UNS-CHAR.
        01  I-H3B         USAGE UNS-CHAR.
        78  I-H3-max      VALUE LENGTH OF Z-H3.
        procedure division.
        goback.
        end program wrapper.


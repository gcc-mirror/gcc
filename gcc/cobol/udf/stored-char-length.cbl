        >> PUSH source format
        >>SOURCE format is fixed

      *  This function is in public domain.
      *  Contributed by James K. Lowden of Cobolworx in August 2024

        Identification Division.
        Function-ID. STORED-CHAR-LENGTH.
        Data Division.
        Linkage Section.
        01 Candidate PIC X Any Length.
        77 Output-Value    PIC 9(8) COMP-5.
        
        Procedure Division using Candidate RETURNING Output-Value.
          Move Function Length( Function Trim(Candidate) )
            to Output-Value. 
        End Function STORED-CHAR-LENGTH.

        >> pop source format
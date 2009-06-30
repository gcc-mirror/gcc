package Discr14 is

  type COMPLETION_CODE is (SUCCESS, FAILURE, NONE);

  type T_SW_TYPE is (NONE, COMPLETION_CODE_TYPE);       

  type T_COMPLETION_CODE_RANGE (CONSTRAINED: BOOLEAN := FALSE) is
  record
     case CONSTRAINED is
        when TRUE =>
           FIRST  : COMPLETION_CODE := SUCCESS;
           LAST   : COMPLETION_CODE := FAILURE;
        when FALSE =>
           null;
     end case;
  end record;

  type T_SW_DIMENSIONS is range 0 .. 3;

  type T_SW_INDEX_LIST is array (T_SW_DIMENSIONS range <>) of POSITIVE;

  type T_SW_TYPE_DESCRIPTOR (SW_TYPE   :  T_SW_TYPE       := NONE;
                             DIMENSION :  T_SW_DIMENSIONS := 0)  is
  record
     BOUNDS : T_SW_INDEX_LIST (1 .. DIMENSION);

     case SW_TYPE is

        when COMPLETION_CODE_TYPE  =>
           COMPLETION_CODE_RANGE   : T_COMPLETION_CODE_RANGE;

        when OTHERS  =>
           null;

     end case;
  end record;

  type SW_TYPE_INFO is access T_SW_TYPE_DESCRIPTOR;

  procedure ASSIGN(TARGET : in out SW_TYPE_INFO; SOURCE : in SW_TYPE_INFO) ;

end Discr14;

with Deferred_Const4_Pkg;

package Deferred_Const4 is

  type R1 is tagged record
    I1 : Integer;
  end record;

  type R2 is new R1 with record
    I2 : Integer;
  end record;

  package My_Q is new Deferred_Const4_Pkg (R2);

  function F return My_Q.T;

end Deferred_Const4;

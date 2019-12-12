package Access5 is
  type Vec;
  type Ptr is access all Vec;
  type Vec is array (1..3) of Ptr;
  function F return Ptr;
  pragma Import (Ada, F);
  Tail : Vec := (F, F, F);

  procedure Dummy;
end;

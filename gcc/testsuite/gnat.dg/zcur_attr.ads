package ZCUR_Attr is
   function F return Integer;
   pragma Machine_Attribute (F, "zero_call_used_regs", "all");
end ZCUR_Attr;


--  { dg-do compile }

package body ref_type is
  type T is tagged null record;
  procedure Print (X : T) is                                   
  begin                                                        
     null;
  end;
end ref_type;

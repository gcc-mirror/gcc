with System; 
package Self is 
  type Lim is limited private;
  type Lim_Ref is access all Lim;
  function G (X : Integer) return lim;

  procedure Change (X : in out Lim; Incr : Integer);
  function Get (X : Lim) return Integer;
private 
  type Lim is limited record
     Comp : Integer;
     Self_Default : Lim_Ref := Lim'Unchecked_Access;
     Self_Unrestricted_Default : Lim_Ref := Lim'Unrestricted_Access;
     Self_Anon_Default : access Lim := Lim'Unchecked_Access;
     Self_Anon_Unrestricted_Default : access Lim := Lim'Unrestricted_Access;
  end record; 
end Self;

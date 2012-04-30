package Warn6 is

  package Q is
    type T is private; -- this is the trigger
  private
    type T is access Integer;
    pragma No_Strict_Aliasing (T);

  end Q;

  subtype Q_T is Q.T;

  procedure Dummy;

end Warn6;

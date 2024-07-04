package Opt103 is

  type Mode is (Stop, Forward, Backward, Up, Down);

  function Get return String;
  pragma Import (Ada, Get);

  function Read return Mode;

  function Translate (S : String) return Mode;

end Opt103;

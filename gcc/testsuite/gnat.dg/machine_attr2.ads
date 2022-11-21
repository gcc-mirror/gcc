package Machine_Attr2 is

  type Non_Secure is access procedure;
  pragma Machine_Attribute (Non_Secure, "cmse_nonsecure_call");

  procedure Call (Proc : Non_Secure);

end Machine_Attr2;

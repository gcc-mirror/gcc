call (foo, a)
     int (**foo) ();
{

  (foo)[1] = call;

  foo[a] (1);
}

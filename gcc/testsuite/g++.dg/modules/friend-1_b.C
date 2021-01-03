// { dg-additional-options "-fmodules-ts -fdump-lang-module" }

module bob;

int peeker::peek (void *data)
{
  return reinterpret_cast <secret *> (data)->key;
}

// { dg-final { scan-lang-dump {Class '::hidey@bob:.' befriending record_type:'::peeker@bob:.'} module } }

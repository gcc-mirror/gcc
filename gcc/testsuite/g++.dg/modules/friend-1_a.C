// { dg-additional-options "-fmodules-ts -fdump-lang-module-blocks" }

export module bob;
// { dg-module-cmi bob }

export struct peeker
{
  static int peek (void *);
};


export class hidey 
{
protected:
  int key;

public:
  hidey (int key) :key (key)
  {
  }
  
  friend class peeker;
};

export class secret : public hidey
{
public:
  secret (int key) : hidey (key)
  {
  }
};

// hidey, peeker & secret are all in different clusters

// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=decl definition '::peeker@bob:.'\n  \[1\]=binding '::peeker'} module } }
// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=decl definition '::hidey@bob:.'\n(  \[.\]=clone (declaration|definition) '::hidey[^\n]* @bob:.'\n)*  \[.\]=binding '::hidey'} module } }
// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=decl definition '::secret@bob:.'\n(  \[.\]=clone (declaration|definition) '::secret[^\n]* @bob:.'\n)*  \[.\]=binding '::secret'} module } }

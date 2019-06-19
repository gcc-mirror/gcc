// { dg-additional-options "-fmodules-ts -fdump-lang-module-blocks" }

export module bob;
// { dg-module-bmi bob }

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

// hidey, peeker & secreat are all in different clusters

// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=decl definition '::peeker@bob:1'\n  \[1\]=binding '::peeker'} module } }
// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=decl definition '::hidey@bob:1'\n  \[1\]=binding '::hidey'} module } }
// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=decl definition '::secret@bob:1'\n  \[1\]=binding '::secret'} module } }

// { dg-additional-options "-fmodules-ts -fdump-lang-module-blocks" }
export module foo;
// { dg-module-cmi foo }


export enum class file_type : signed char { none = 0 };

export class directory_entry
{
public:
  directory_entry(int);
    
  int _M_path;

  // ICE from deferred_parse NSDMI in as_base class
  file_type _M_type = file_type::none;
};

// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=decl definition '::directory_entry'\n  \[1\]=decl definition '::directory_entry::__as_base '\n  \[2\]=decl declaration '::directory_entry::__ct '\n} module } }

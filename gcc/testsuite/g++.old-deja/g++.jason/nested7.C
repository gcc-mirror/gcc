// { dg-do assemble  }
// Testcase for defining nested types separately.

class remote
{
  class remote_file;
};

class remote::remote_file
{
public:
  ~remote_file();
};

remote::remote_file::~remote_file()
{}

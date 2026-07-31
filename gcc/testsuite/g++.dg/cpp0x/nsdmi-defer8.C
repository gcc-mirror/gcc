// PR c++/126481
// { dg-do compile { target c++11 } }

struct QPluginMetaData
{
  static constexpr int CurrentMetaDataVersion = 1;

  struct Header {
    int version = CurrentMetaDataVersion;
  };

  struct MagicHeader {
    Header header = {};
  };
};

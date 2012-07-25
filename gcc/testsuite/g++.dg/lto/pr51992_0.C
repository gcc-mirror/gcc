// PR c++/51992
// { dg-lto-do assemble }

template<typename Enum>
class QFlags
{
    int i;
public:
    inline QFlags(Enum f) : i(f) {}
};
class QString {};
class KComponentData;
class KConfig
{
public:
    enum OpenFlag {
        IncludeGlobals = 0x01,
	CascadeConfig = 0x02,
	FullConfig = IncludeGlobals|CascadeConfig
    };
    typedef QFlags<OpenFlag> OpenFlags;
};
template <class T>
class KSharedPtr {};
class KSharedConfig : public KConfig
{
public:
  typedef KSharedPtr<KSharedConfig> Ptr;
    static KSharedConfig::Ptr openConfig(const QString& fileName = QString(),
				         OpenFlags mode = FullConfig,
					 const char *resourceType = "config");
    static KSharedConfig::Ptr openConfig(const KComponentData &componentData,
				         const QString &fileName = QString(),
                                         OpenFlags mode = FullConfig,
					 const char *resourceType = "config");
};
typedef KSharedConfig::Ptr KSharedConfigPtr;
namespace KGlobal
{
    KComponentData &mainComponent();
};
KSharedConfigPtr KSharedConfig::openConfig(const QString& fileName,
                                           OpenFlags flags,
                                           const char *resType)
{
    return openConfig(KGlobal::mainComponent(), fileName, flags, resType);
}
KSharedConfigPtr KSharedConfig::openConfig(const KComponentData &componentData,
                                           const QString& fileName,
                                           OpenFlags flags,
                                           const char *resType)
{
    return KSharedConfigPtr();
}

// PR ipa/93166
// { dg-lto-do link }
// { dg-lto-options { { -fPIC -O2 -flto -fvisibility=hidden } } }
// { dg-require-effective-target shared }
// { dg-require-effective-target fpic }
// { dg-extra-ld-options "-shared" }
// { dg-require-visibility "" }

namespace Qt {
enum DropAction {};
}
class QObject;
struct QMetaObject {
  static void activate(const QMetaObject *, void *);
  enum Call {};
  struct {
    const QMetaObject *superdata;
    int *stringdata;
    unsigned *data;
    typedef void (*StaticMetacallFunction)(QObject *, Call, int, void **);
    StaticMetacallFunction static_metacallrelatedMetaObjectsextradata;
  } d;
};
class QString;
struct QListData {
  struct Data;
  Data *d;
};
template <typename> class QList {
  union {
    QListData p;
    QListData::Data *d;
  };

public:
  ~QList();
};
class QStringList : QList<QString> {};
template <typename> struct QScopedPointerDeleter;
class QObjectData;
template <typename, typename = QScopedPointerDeleter<QObjectData>>
class QScopedPointer {
public:
  ~QScopedPointer();
  QObjectData *d;
};

template <typename> struct FunctionPointer;
template <class Obj, typename Ret, typename... Args>
struct FunctionPointer<Ret (Obj::*)(Args...)> {
  typedef Obj Object;
};

class QObject {
public:
  virtual ~QObject();
  virtual void disconnectNotify();
  template <typename Func1, typename Func2>
  void connect(typename FunctionPointer<Func1>::Object *, Func1,
               typename FunctionPointer<Func2>::Object *, Func2);
  QScopedPointer<QObjectData> d_ptr;
};
class QPaintDevicePrivate;
class QPaintDevice {
public:
  virtual ~QPaintDevice();
  unsigned short painters;
  QPaintDevicePrivate *reserved;
};
class QWidgetData;
class QWidget : public QObject, QPaintDevice {
  QWidgetData *data;
};
class QFrame : public QWidget {};
class QMenu;
class QMimeData;
class QAbstractScrollArea : public QFrame {};
class QAbstractItemView : public QAbstractScrollArea {};
class QTreeView : public QAbstractItemView {};
class QTreeWidgetItem;
class QTreeWidget : public QTreeView {};
class QSignalMapper;
class KActionCollection;
class MenuFile;
class MenuFolderInfo;
class MenuEntryInfo;
class MenuSeparatorInfo;
class TreeView : QTreeWidget {
public:
  static const QMetaObject d;
  static void qt_static_metacall(QObject *, QMetaObject::Call, int, void **);
  void disableAction();
  bool dropMimeData(QTreeWidgetItem *, int, const QMimeData *, Qt::DropAction);
  KActionCollection *m_ac;
  QMenu *m_popupMenu;
  int m_clipboard;
  MenuFolderInfo *m_clipboardFolderInfo;
  MenuEntryInfo *m_clipboardEntryInfo;
  bool m_showHidden;
  MenuFile *m_menuFile;
  MenuFolderInfo *m_rootFolder;
  MenuSeparatorInfo *m_separator;
  QStringList m_newMenuIds;
  QStringList m_newDirectoryList;
  bool m_layoutDirty;
  bool m_detailedMenuEntries;
  bool m_detailedEntriesNamesFirst;
  QStringList m_dropMimeTypes;
  QSignalMapper *m_sortSignalMapper;
};
struct {
  int n, data[];
} b;
unsigned c[]{};
void TreeView::qt_static_metacall(QObject *p1, QMetaObject::Call, int,
                                  void **p4) {
  static_cast<TreeView *>(p1)->dropMimeData(
      0, 0, 0, *reinterpret_cast<Qt::DropAction *>(p4));
}
const QMetaObject TreeView::d{&d, b.data, c, qt_static_metacall};
void TreeView::disableAction() { QMetaObject::activate(&d, nullptr); }
template <typename> struct QScopedPointerDeleter;
class KXMLGUIClientPrivate;
class KXMLGUIClient {
public:
  virtual void m_fn2();
  KXMLGUIClient();
  virtual ~KXMLGUIClient();
  KXMLGUIClientPrivate *const d;
};
class KXMLGUIBuilderPrivate;
class KXMLGUIBuilder {
public:
  virtual ~KXMLGUIBuilder();
  virtual QStringList customTags();
  KXMLGUIBuilderPrivate *const d;
};

class QMainWindow : public QWidget {};
class KMainWindowPrivate;
class KMainWindow : public QMainWindow {
  KMainWindowPrivate *const k_ptr;
};
class KXmlGuiWindow : public KMainWindow,
                      KXMLGUIBuilder,
                      virtual KXMLGUIClient {
public:
  KXmlGuiWindow();
};
class QSplitter;
class QAction;
class BasicTab;
class TreeView;
class KTreeWidgetSearchLine;
class KMenuEdit : public KXmlGuiWindow {
public:
  KMenuEdit();
  TreeView *m_tree;
  BasicTab *m_basicTab;
  QSplitter *m_splitter;
  KTreeWidgetSearchLine *m_searchLine;
  QAction *m_actionDelete;
  bool m_showHidden;
};
int __attribute__((visibility("default"))) fn1() { new KMenuEdit; return 0; }
template <class> struct QTypedArrayData;
class QString {
public:
  ~QString();
  QTypedArrayData<unsigned short> *d;
};
class MenuEntryInfo;
class MenuInfo {
public:
  virtual ~MenuInfo();
};
class MenuFolderInfo : MenuInfo {
public:
  void updateFullId(const QString &);
  QString id;
  QString fullId;
  QString caption;
  QString genericname;
  QString comment;
  QString directoryFile;
  QString icon;
  QList<MenuFolderInfo *> subFolders;
  QList<MenuEntryInfo *> entries;
  QList<MenuInfo *> initialLayout;
  bool dirty;
  bool hidden;
};
void MenuFolderInfo::updateFullId(const QString &) {
  for (MenuFolderInfo *a = 0;;)
    a->updateFullId(fullId);
}

MenuFolderInfo *a1, *b1;
bool TreeView::dropMimeData(QTreeWidgetItem *, int, const QMimeData *,
                            Qt::DropAction) {
  b1->updateFullId(a1->fullId);
  return true;
}

class BasicTab {
public:
  void slotDisableAction();
};

KMenuEdit::KMenuEdit() {
  connect(m_tree, &TreeView::disableAction, m_basicTab,
          &BasicTab::slotDisableAction);
}

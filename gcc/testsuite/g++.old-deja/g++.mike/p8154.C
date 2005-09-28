// { dg-do assemble  }
// { dg-options "-w -fpermissive" }
// prms-id: 8154

class QvFieldData;
class QvNode {
    QvFieldData *fieldData;
};
class QvGroup : public QvNode {
  static QvFieldData *fieldData;
};
class QvUnknownNode : public QvGroup
{
public:
  QvUnknownNode :: QvUnknownNode ();
private:
  static QvFieldData *fieldData;
  virtual QvFieldData *getFieldData() { return fieldData; }
};

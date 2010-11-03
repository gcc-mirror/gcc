class btIDebugDraw;
class btCollisionWorld {
    virtual btIDebugDraw* getDebugDrawer()  { };
    static void rayTestSingle();
};
class btTriangleCallback {
public:
    virtual ~btTriangleCallback();
};
class btTriangleRaycastCallback: public btTriangleCallback {
public:
    btTriangleRaycastCallback();
};
void btCollisionWorld::rayTestSingle()
{
  struct BridgeTriangleRaycastCallback : public btTriangleRaycastCallback {
      BridgeTriangleRaycastCallback() : btTriangleRaycastCallback() { }
  };
}

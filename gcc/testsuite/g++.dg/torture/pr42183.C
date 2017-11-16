// { dg-do compile }
// { dg-additional-options "-Wno-return-type" }

class IntSize {
public:
    IntSize(int width, int height) : m_width(width), m_height(height) { }
    int m_width, m_height;
};
class IntPoint {
public:
    IntPoint(int x, int y) : m_x(x), m_y(y) { }
    int m_x, m_y;
};
class IntRect {
public:
    IntRect(int x, int y, int width, int height)
        : m_location(IntPoint(x, y)), m_size(IntSize(width, height)) { }
    void intersect(const IntRect&);
    IntPoint m_location;
    IntSize m_size;
};
inline IntRect intersection(const IntRect& a, const IntRect& b) {
    IntRect c = a;
    c.intersect(b);
    return c;
}
class RenderObject  {
public:
    int contentWidth() const { }
    int contentHeight() const { }
    virtual int xPos() const { }
    virtual int yPos() const { }
    virtual int paddingTop() const;
    virtual int paddingLeft() const;
    virtual int borderTop() const { }
    virtual int borderLeft() const { }
};
class RenderMenuList : public RenderObject {
    virtual IntRect controlClipRect(int tx, int ty) const;
    RenderObject* m_innerBlock;
};
IntRect RenderMenuList::controlClipRect(int tx, int ty) const {
    IntRect outerBox(tx + borderLeft() + paddingLeft(),
                     ty + borderTop() + paddingTop(),
                     contentWidth(), contentHeight());
    IntRect innerBox(tx + m_innerBlock->xPos() + m_innerBlock->paddingLeft(),
                     ty + m_innerBlock->yPos() + m_innerBlock->paddingTop(),
                     m_innerBlock->contentWidth(),
                     m_innerBlock->contentHeight());
    return intersection(outerBox, innerBox);
}


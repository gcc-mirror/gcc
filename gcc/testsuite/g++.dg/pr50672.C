/* { dg-do compile } */
/* { dg-options "-O2 -ftree-tail-merge" } */
typedef int BoxCoordinate;
typedef int BoxDimension;
const BoxDimension X = 0;
const BoxDimension Y = 1;
const BoxDimension NDimensions = 2;
class BoxPoint {
    BoxCoordinate point[NDimensions];
public:
    bool isValid() const;
    void operator += (const BoxPoint& p)     {
        if (isValid() && p.isValid())  {
            point[X] += p.point[X];
        }
    }
    const BoxCoordinate& operator [] (const BoxDimension& dimension) const {
        return point[dimension];
    }
};
class BoxRegion {
public:
    BoxCoordinate& origin(BoxDimension d) const;
    BoxCoordinate& space(BoxDimension d) const;
};
inline bool operator <= (const BoxPoint& p, const BoxRegion& r) {
    for (BoxDimension d = X;
         d <= Y;
         d++)  if (p[d] < r.origin(d) || p[d] >= r.origin(d) + r.space(d))     
return false;
    return true;
}
typedef struct _WidgetRec *Widget;
struct GraphGC {
    BoxPoint offsetIfSelected;
};
class GraphNode;
class GraphEdge {
public:
    GraphNode *from() const;
    GraphNode *to() const;
};
class LineGraphEdge: public GraphEdge {
protected:
    virtual void drawLine(Widget w,      const GraphGC& gc) const;
    void _print(const GraphGC &gc) const;
};
class ArcGraphEdge: public LineGraphEdge {
    static bool center(const BoxPoint& p1, const BoxPoint& p2,
                       const BoxPoint& p3, double& x, double& y);
    void makeLine(Widget w,     const GraphGC& gc) const;
};
class GraphNode {
public:
    bool& selected();
    GraphEdge *firstTo() const;
    GraphEdge *nextTo(GraphEdge *ref) const;
    virtual const BoxPoint& pos() const = 0;
    virtual const BoxRegion& region(const GraphGC& gc) const = 0;
    virtual bool isHint() const;
};
class PosGraphNode: public GraphNode { };
class RegionGraphNode: public PosGraphNode { };
class HintGraphNode: public RegionGraphNode { };
void ArcGraphEdge::makeLine(Widget w, const GraphGC& gc) const {
    HintGraphNode *arc_hint = 0;
    RegionGraphNode *arc_from = 0;
    RegionGraphNode *arc_to = 0;
    bool make_arc = true;
    if (from()->isHint() && to()->isHint())     {
        make_arc = false;
    }
    else if (from()->isHint() && from()->firstTo() != 0)     {
        if (arc_hint == 0 || arc_from == 0 || arc_to == 0
            || arc_hint->nextTo(arc_hint->firstTo()) != 0)  {
            make_arc = false;
        }
    }
    if (!make_arc)     {
        if (w != 0)      LineGraphEdge::drawLine(w, gc);
        else      LineGraphEdge::_print(gc);
        return;
    }
    BoxPoint pos_from = arc_from->pos();
    BoxRegion region_from = arc_from->region(gc);
    BoxPoint pos_to = arc_to->pos();
    BoxRegion region_to = arc_to->region(gc);
    BoxPoint pos_hint = arc_hint->pos();
    if (arc_hint->selected())     {
        pos_hint += gc.offsetIfSelected;
    }
    if (pos_hint <= region_from || pos_hint <= region_to)     {
        return;
    }
    double cx, cy;
    bool ok = center(pos_from, pos_hint, pos_to, cx, cy);
    if (!ok)     {
        if (w != 0)      LineGraphEdge::drawLine(w, gc);
        else      LineGraphEdge::_print(gc);
    }
}
